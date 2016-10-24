{-# LANGUAGE ExistentialQuantification #-}

module Hardware.PipelineDSL.Pipeline (
    sig,
    stage,
    HW (..),
    rHW,
    Signal (..),
    PStage (..),
    SigMap (..),
    MOps (..),
    BOps (..),
    UOps (..),
    Reg (..),
    simplify,
    getSignalWidth
) where

import Control.Monad
import Control.Applicative
import Data.Monoid ( (<>) )
import Control.Monad.Fix
import Data.Ix (range)
import Data.Bits (finiteBitSize, countLeadingZeros)

import Debug.Trace

data PStage = PStage { pipeStageId :: Int
                     , pipeStageSignal :: Signal
                     , pipeStageStageNum :: Int
                     , pipeStageUpstreamStages :: [PStage]
                     , pipeStageDownstreamStages :: [PStage]
                     , pipeStageRdy :: Signal
                     , pipeStageVld :: Signal
                     , pipeStageName :: String
                     , pipeStageDelaysNum :: Int
                     , pipeStageDelayRegs :: Int -> PDelayReg
                     , pipeStageReg :: Signal
                     , pipeStageBufferDepth :: Int
                     , pipeStageCtrl :: PipeStageLogic }

data PDelayReg = PDelayReg { delayRegEn :: Signal
                           , delayRegData :: Signal }

data MOps = Or | And | Sum | Mul
data BOps = Sub | Equal | NotEqual
data UOps = Not | Neg | Signum | Abs

data Signal = Alias String Int -- name, width
            | Lit Int Int -- toInteger, width can be fixed or any (value, width)
            | SigRef Int Signal
            | UnaryOp UOps Signal
            | MultyOp MOps [Signal]
            | BinaryOp BOps Signal Signal
            | Cond Signal Signal -- conditional signal valid, value
            | Undef
            -- register output. managed separately outside of the HW monad
            | PipelineStage PStage
            | RegRef Int Reg -- register, inserts 1 clock delay

-- list all pipeline stages that are inputs for a given signal
queryUpstreamStages :: Signal -> [PStage]
queryUpstreamStages (PipelineStage x) = [x]
queryUpstreamStages (SigRef _ s) = queryUpstreamStages s
queryUpstreamStages (MultyOp _ s) = concat $ map queryUpstreamStages s
queryUpstreamStages (BinaryOp _ s1 s2) = (queryUpstreamStages s1) ++ (queryUpstreamStages s2)
queryUpstreamStages (UnaryOp _ s) = queryUpstreamStages s
queryUpstreamStages _ = []

-- longest distance to current stage
-- shorter paths need to be compensated
-- returns 0 if there is no paths connecting specifid stage
downstreamDist :: Int -> PStage -> Int
downstreamDist stgid sig = r stgidPaths where
    -- get all stages and distances
    -- returns an array of pairs: [(distance, stage)]
    allStgsDist sig = c ++ n where
        t = queryUpstreamStages sig
        c = map ((,) 0) t
        n = map (\(x, y) -> (x + 1, y)) $ concat $ map (allStgsDist . pipeStageSignal) t
    stgidPaths = filter (\(_, s) -> (pipeStageId s) == stgid) $ allStgsDist $ pipeStageSignal sig
    r [] = 0
    r x = maximum $ map fst x


getSignalWidth :: Signal -> Int
getSignalWidth (PipelineStage s) = getSignalWidth $ pipeStageSignal s
getSignalWidth (SigRef _ s) = getSignalWidth s
getSignalWidth (MultyOp _ s) = maximum $ map getSignalWidth s
getSignalWidth (BinaryOp Equal _ _) = 1
getSignalWidth (BinaryOp _ s1 s2) = max (getSignalWidth s1) (getSignalWidth s2)
getSignalWidth (UnaryOp _ s) = getSignalWidth s
getSignalWidth (Cond _ s) = getSignalWidth s
getSignalWidth (Lit _ s) = s
getSignalWidth (Alias _ s) = s
getSignalWidth (RegRef _ (Reg s)) = maximum $ map (getSignalWidth . snd) s
getSignalWidth Undef = 0

mapSignal :: (Signal -> Signal) -> Signal -> Signal
mapSignal f s =  mapSignal' (f s) where
    -- mapSignal aplies the transformation, mapSignal' does structure-preserving traversing
    mapSignal' (MultyOp op s) = MultyOp op $ map (mapSignal f) s
    mapSignal' (BinaryOp op s1 s2) = BinaryOp op (mapSignal f s1) (mapSignal f s2)
    mapSignal' (UnaryOp op s) = UnaryOp op (mapSignal f s)
    mapSignal' (SigRef n s) = SigRef n (mapSignal f s)
    mapSignal' (Cond n s) = Cond (mapSignal f n) (mapSignal f s)
    mapSignal' x = x


rewrite :: (Signal -> Signal) -> Signal -> Signal
rewrite f s =  f $ rewrite' (f s) where
    -- rewrite aplies the transformation, rewrite' does structure-preserving traversing
    rewrite' (MultyOp op s) = f $ MultyOp op $ map (rewrite f) s
    rewrite' (BinaryOp op s1 s2) = f $ BinaryOp op (rewrite f s1) (rewrite f s2)
    rewrite' (UnaryOp op s) = f $ UnaryOp op (rewrite f s)
    rewrite' (SigRef n s) = f $ SigRef n (rewrite f s)
    rewrite' (Cond n s) = f $ Cond (rewrite f n) (rewrite f s)
    rewrite' x = x

-- apply some rewrite rules to improve readability of generated verilog
-- use mapSignal to apply rules recursively
simplify :: Signal -> Signal
simplify = rewrite smpl where
    smpl (UnaryOp Not (Lit 1 1)) = Lit 0 1
    smpl (UnaryOp Not (Lit 0 1)) = Lit 1 1
    smpl (UnaryOp Not (UnaryOp Not s)) = s

    smpl (MultyOp Or s) = if (any (not . f0) s) then r1 else r where
        r = case (filter f1 s) of 
            [] -> Lit 1 1
            [x] -> x
            x -> MultyOp Or x

    smpl (MultyOp And s) = r where
        r = case (filter f0 s) of 
            [] -> Lit 1 1
            [x] -> x
            x -> MultyOp And x
 
    smpl x = x

    f1 (Lit 0 1) = False
    f1 _ = True
    f0 (Lit 1 1) = False
    f0 _ = True

    r0 = Lit 0 1
    r1 = Lit 1 1


-- convert user type to Signal representation
-- for example (a, b)
class ToSignal a where
    toSignal :: a -> Signal
    fromSignal :: Signal -> a

instance Num Signal where
    abs = UnaryOp Abs
    negate = UnaryOp Neg
    (*) x y = MultyOp Mul [x, y]
    (+) x y = MultyOp Sum [x, y]
    (-) = BinaryOp Sub
    signum = UnaryOp Signum
    fromInteger x = Lit (fromInteger x) 32

type RefSt a = [(Int, a)]
data Reg = Reg [(Signal, Signal)]
data SigMap = SigMap { smSignals :: RefSt Signal
                     , smStages :: RefSt PStage
                     , smRegs ::RefSt Reg }

--generic Monoid?
instance Monoid SigMap where
    mempty = SigMap [] [] []
    mappend (SigMap s1 s2 s3) (SigMap s1' s2' s3') = SigMap (s1 <> s1') (s2 <> s2') (s3 <> s3')

data HW p s = HW { runHW :: Int -> PipeCtrl -> (Int, SigMap, s) } | HWEmpty

data PipeCtrl = PipeCtrl { pipeCtrlStages :: [(Int, PStage)]
                         , pipeCtrlDelayReg :: (Int -> Int -> Signal)}

data PipeStageLogic = PipeStageLogic { pslRdy  :: Signal
                                     , pslDep  :: Signal
                                     , pslDe   :: Signal
                                     , pslRdyn :: Signal
                                     , pslTake :: Signal -- flop enable signal
                                     , pslTakeNext :: Signal
                                     , pslDrop :: Signal
                                     , pslDropNext :: Signal
                                     , pslClr :: Signal }

rHW am@(HW m) = sm' where
    (_, sm, _) = m 0 pipectrl
    stgs = smStages sm
    pipectrl = PipeCtrl stgs queryDelayRegs

    (HW m') = do
        am
        forM stgs f' where -- generate delayed stages
            f' (sid, stg) = g (pipeStageDelaysNum stg) sid stg
            g n sid stg = do
                r <- foldMapM f [1..n] (0, (pipeStageSignal stg))
                return $ (sid, [(0, pipeStageReg stg)] ++ r)
            f i s = ((,) i) <$> (stage (snd s))

    fstIsfilt c l = snd $ head $ filter filt l where
        filt (x, _) = x == c

    queryDelayRegs srcid mystgid = fstIsfilt mystgid $ fstIsfilt srcid delayRegs

    (_, sm', delayRegs) = m' 0 pipectrl

instance Functor (HW p) where
    fmap func (HW m) = HW t where
        t i sm = (i', signals, load) where
            (i', signals, old_load) = m i sm
            load = func old_load

instance Applicative (HW p) where
    pure b = HW $ \x -> (\_ -> (x, mempty, b))
    (<*>) (HW f) (HW o) = HW fr where
        fr i sm = (xn, cl <> cr, sa p) where
            (na, cl, sa) = f i sm
            (xn, cr, p) = o na sm

instance Monad (HW p) where
    return = pure
    (>>=) (HW a) b = HW f where
        f n sm = (ni, ca <> cb, si) where
            (na, ca, sa) = a n sm
            (HW bu) = b sa
            (ni, cb, si) = bu na sm

instance Alternative (HW p) where
    empty = HWEmpty
    (<|>) = (>>)

instance MonadPlus (HW p) where
    mzero = HWEmpty
    mplus = (>>)

instance MonadFix (HW p) where
    mfix m = HW f where
        f i p = (runHW (m a)) i p where
            (_, _, a) = f i p

-- creates reference
sig :: Signal -> HW a Signal
sig inputSignal = HW l where
    l nsig _ = (nsig + 1, mempty {smSignals = [(nsig, inputSignal)]}, s) where
        s = SigRef nsig inputSignal

mkReg :: [(Signal, Signal)] -> HW a Signal
mkReg reginput = HW f where
    f nsig _ = (nsig + 1, mempty {smRegs = [(nsig, r)]}, s) where
        r = Reg reginput
        s = RegRef nsig r

stage :: Signal -> HW a Signal
stage = stage' 0

stageControl nsig vld upstreamStages downstreamStages = (ctrl, dereg) where
    or' = MultyOp Or
    and' = MultyOp And
    not' = UnaryOp Not

    dereg = Reg [(take', Lit 1 1), (declr, Lit 0 1)]
    deregref = RegRef (nsig + 2) dereg

    rdy = or' $ map pipeStageRdy $ upstreamStages
    dep' = and' $ map (pslDe . pipeStageCtrl) $ upstreamStages
    drp = not' vld
    take' = and' [rdy, dep', not' drp]
    takenext' = and' $ map (pslTake . pipeStageCtrl) $ downstreamStages
    dropnext' = and' $ map (pslDrop . pipeStageCtrl) $ downstreamStages
    declr = and' [not' take', (or' [takenext', dropnext'])]

    ctrl = PipeStageLogic
        { pslRdy = rdy
        , pslDep = dep'
        , pslDe  = deregref
        , pslRdyn = and' $ map (pslRdy . pipeStageCtrl) $ downstreamStages
        , pslTake = take'
        , pslTakeNext = takenext'
        , pslDrop = drp
        , pslDropNext = dropnext'
        , pslClr = and' [not' take', (or' [takenext', dropnext'])] }

stage' bufferdepth inputSignal' = HW l where
    l nsig pipectrl = (nsig + 3, me, self) where
        self = PipelineStage stg
        stg = PStage    { pipeStageId = nsig
                        , pipeStageSignal = inputSignal
                        , pipeStageStageNum = stageid
                        , pipeStageUpstreamStages = upstreamStages
                        , pipeStageDownstreamStages = downstreamStages
                        , pipeStageRdy = rdySignal
                        , pipeStageVld = vld
                        , pipeStageName = name
                        , pipeStageDelaysNum = ndelays
                        , pipeStageDelayRegs = delayedRegs
                        , pipeStageReg = (RegRef (nsig + 1) reg)
                        , pipeStageBufferDepth = bufferdepth
                        , pipeStageCtrl = ctrl }

        -- take care of conditional signal
        (vld, inputSignal) = case inputSignal' of
            (Cond v s) -> ((mapSignal mapR v), s)
            _ -> (Lit 1 1, inputSignal')

        stgs = pipeCtrlStages pipectrl
        enablesignal = pslTake ctrl
        clearsignal = pslClr ctrl
        reg = Reg [(enablesignal, delayedInputsSignal), (clearsignal, Undef)]

        ndelays = maximum $ map (downstreamDist nsig) $ map snd stgs

        me = mempty { smStages = [(nsig, stg)]
                    , smRegs = [(nsig + 1, reg), (nsig + 2, dereg)] }
 
        upstreamStages = queryUpstreamStages inputSignal

        stageid = case map pipeStageStageNum $ upstreamStages of
            [] -> 0
            x -> 1 + (maximum x)

        allStagesInPipeline = map snd stgs
        
        hasMeUpstream s = elem nsig $ map pipeStageId $ pipeStageUpstreamStages s
        downstreamStages = filter hasMeUpstream allStagesInPipeline

        -- for a given stage, return ready and data signals
        delayedRegs 0 = PDelayReg rdySignal self
        delayedRegs n = PDelayReg rdySignal self
       
        name = "stg_" ++ (show nsig)

        -- map only the stages we depend on
        delayedInputsSignal = mapSignal mapR inputSignal
        mapR (PipelineStage p) = (pipeCtrlDelayReg pipectrl) (pipeStageId p) stageid
        mapR x = x

        -- enable signal stageid
        rdySignal = Lit 1 1

        -- pipeline control logic
        (ctrl, dereg) = stageControl nsig vld upstreamStages downstreamStages

representationWidth :: Int -> Int
representationWidth i = (finiteBitSize i) - (countLeadingZeros i)

-- iterate over [x] in Monad context
-- passes results of each iteration to the next one
-- returns results of all actions in a list
foldMapM _ [] _ = pure []
foldMapM f [x] p = do
    t <- f x p
    return [t]
foldMapM f (x:xn) p = do
    t <- f x p
    ((:) t) <$> foldMapM f xn t
