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
                     , pipeStageReg :: Signal }

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

data Reg = Reg [(Signal, Signal)]

data SigMap = SigMap { smSignals :: [(Int, Signal)]
                     , smStages :: [(Int, PStage)]
                     , smRegs :: [(Int, Reg)] }

--generic Monoid?
instance Monoid SigMap where
    mempty = SigMap [] [] []
    mappend (SigMap s1 s2 s3) (SigMap s1' s2' s3') = SigMap (s1 <> s1') (s2 <> s2') (s3 <> s3')

data HW p s = HW { runHW :: Int -> PipeCtrl -> (Int, SigMap, s) } | HWEmpty

data PipeCtrl = PipeCtrl { pipeCtrlStages :: [(Int, PStage)]
                         , pipeCtrlLength :: Int
                         , pipeCtrlStageCtrl :: (Int -> PipeStageLogic)
                         -- source stage id -> dst stage id -> delayed value   
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
    l = maximum $ map pipeStageStageNum $ map snd stgs
    queryPipeCtrl = (!!) pipectrlsignals
    pipectrl = PipeCtrl stgs l queryPipeCtrl queryDelayRegs

    (HW m') = do
        am
        (,) <$> genPipeCtrl <*> genDelays
    
    stagesWithId i = map snd $ filter (\s -> i == pipeStageStageNum (snd s)) stgs

    genPipeCtrl = mfix $ \pl -> do
        let rdyn = (map pslRdy $ tail pl) ++ [Lit 1 1]
        let takenext = (map pslTake $ tail pl) ++ [Lit 1 1]
        let dep = [Alias "data1en" 1] ++ (map pslDe $ init pl)
        let dropnext = (map pslDrop $ tail pl) ++ [Lit 1 1]

        let or' = MultyOp Or
        let and' = MultyOp And
        let not' = UnaryOp Not

        forM [0 .. l] $ \i -> do
            let rdy = or' $ map pipeStageRdy $ stagesWithId i
            let drp = not' $ and' $ map pipeStageVld $ stagesWithId i
            let dep' = dep !! i
            let takenext' = takenext !! i
            let rdyn' = rdyn !! i
            let take' = and' [rdy, dep', not' drp]
            let dropnext' = dropnext !! i
            let declr = and' [not' take', (or' [takenext', dropnext'])]
            de <- mkReg [(take', Lit 1 1), (declr, Lit 0 1)]
            return $ PipeStageLogic rdy dep' de rdyn' take' takenext' drp dropnext' declr

    genDelays = do
        forM stgs $ \(i, stg) -> do
            let n = pipeStageDelaysNum stg
            ((,) i) <$> genDelayRegs n stg

    foldMapM f [x] p = do
        t <- f x p
        return [t]
    foldMapM f (x:xn) p = do
        t <- f x p
        ((:) t) <$> foldMapM f xn t

    -- generate delay regs for each pipeline stage
    genDelayRegs 0 stg = return [(pipeStageStageNum stg + 1, pipeStageReg stg)]
    genDelayRegs 1 stg = do
        t <- fstDelayReg stg
        return [((pipeStageStageNum stg) + 1, pipeStageReg stg), ((pipeStageStageNum stg) + 2, t)]
    genDelayRegs n stg = do
        let stgids = map ((+) (pipeStageStageNum stg)) [2 .. n]
        let ensigs = map (pslTake . queryPipeCtrl) stgids
        let mkReg1 en v = mkReg [(en, v)]
        [d0, d1] <- genDelayRegs 1 stg
        tailD <- (zip (map ((+) 1) stgids)) <$> foldMapM mkReg1 ensigs (snd d1)
        return $ [d0, d1] ++ tailD

    fstDelayReg stg = mkReg [(
        pslTake $ queryPipeCtrl $ pipeStageStageNum stg + 1,
            pipeStageReg stg)]
    
    fstIsfilt c l = snd $ head $ filter (\x -> (fst x) == c) l
    queryDelayRegs srcid mystgid = fstIsfilt mystgid $ fstIsfilt srcid delayRegs

    (_, sm', (pipectrlsignals, delayRegs)) = m' 0 pipectrl

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
stage inputSignal' = HW l where
    l nsig pipectrl = (nsig + 2, me, self) where
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
                        , pipeStageReg = (RegRef (nsig + 1) reg) }

        -- take care of conditional signal
        (vld, inputSignal) = case inputSignal' of
            (Cond v s) -> ((mapSignal mapR v), s)
            _ -> (Lit 1 1, inputSignal')
    
        stgs = pipeCtrlStages pipectrl
        enablesignal = pslTake $ (pipeCtrlStageCtrl pipectrl) stageid
        clearsignal = pslClr $ (pipeCtrlStageCtrl pipectrl) stageid
        reg = Reg [(enablesignal, delayedInputsSignal), (clearsignal, Undef)]


        me = mempty { smStages = [(nsig, stg)]
                    , smRegs = [(nsig + 1, reg)] }
 
        upstreamStages = queryUpstreamStages inputSignal

        stageid = case map pipeStageStageNum $ upstreamStages of
            [] -> 0
            x -> 1 + (maximum x)

        allStagesInPipeline = map snd stgs
        
        hasMeUpstream s = elem nsig $ map pipeStageId $ pipeStageUpstreamStages s
        downstreamStages = filter hasMeUpstream allStagesInPipeline

        ndelays = case (map pipeStageStageNum downstreamStages) of
            [] -> 0
            x -> (maximum x) - stageid - 1

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
