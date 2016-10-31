{-# LANGUAGE ExistentialQuantification #-}

module Hardware.PipelineDSL.Pipeline (
    sig,
    sigp,
    stage,
    HW (..),
    PipeM (..),
    Signal (..),
    PStage (..),
    SigMap (..),
    StgMap (..),
    MOps (..),
    BOps (..),
    UOps (..),
    Reg (..),
    simplify,
    getSignalWidth,
    rPipe
) where

import Control.Monad
import Control.Applicative
import Data.Monoid ( (<>) )
import Control.Monad.Fix
import Data.Ix (range)
import Data.Bits (finiteBitSize, countLeadingZeros)
import Control.Monad.RWS.Lazy hiding (Sum)

import Debug.Trace

data PStage = PStage { pipeStageId :: Int
                     , pipeStageSignal :: Signal
                     , pipeStageStageNum :: Int
                     , pipeStageUpstreamStages :: [PStage]
                     , pipeStageDownstreamStages :: [PStage]
                     , pipeStageRdy :: Signal
                     , pipeStageName :: String
                     , pipeStageDelaysNum :: Int
                     , pipeStageBufferDepth :: Int
                     , pipeStageLogicStages :: [Signal] }

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
            | Stage LogicStage
            -- register output. managed separately outside of the HW monad
            | PipelineStage PStage
            | RegRef Int Reg -- register, inserts 1 clock delay

-- list all pipeline stages that are inputs for a given signal
queryUpstreamLStages :: Signal -> [LogicStage]
queryUpstreamLStages (Stage x) = [x]
queryUpstreamLStages (SigRef _ s) = queryUpstreamLStages s
queryUpstreamLStages (MultyOp _ s) = concat $ map queryUpstreamLStages s
queryUpstreamLStages (BinaryOp _ s1 s2) = (queryUpstreamLStages s1) ++ (queryUpstreamLStages s2)
queryUpstreamLStages (UnaryOp _ s) = queryUpstreamLStages s
queryUpstreamLStages _ = []

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
                     , smRegs ::RefSt Reg }
data StgMap = StgMap {smStages :: RefSt PStage}

instance Monoid SigMap where
    mempty = SigMap [] []
    mappend (SigMap s1 s2) (SigMap s1' s2') = SigMap (s1 <> s1') (s2 <> s2')

instance Monoid StgMap where
    mempty = StgMap []
    mappend (StgMap s) (StgMap s') = StgMap (s <> s')

type HW = RWS () SigMap Int
type PipeM = RWST PipeCtrl StgMap Int HW

data PipeCtrl = PipeCtrl { pipeCtrlStages :: [(Int, PStage)]}

data PipeStageLogic = PipeStageLogic { pslRdy  :: Signal
                                     , pslDep  :: Signal
                                     , pslDe   :: Signal
                                     , pslRdyn :: Signal
                                     , pslTake :: Signal -- flop enable signal
                                     , pslTakeNext :: Signal
                                     , pslDrop :: Signal
                                     , pslDropNext :: Signal
                                     , pslClr :: Signal }

rPipe f = (a', sigs, sm) where
    stgs = smStages sm
    pipectrl = PipeCtrl stgs
    m = runRWST f pipectrl 0 -- Pipe
    r@((a', _, sm), _, sigs) = runRWS m () 0 -- HW

-- creates reference
sig :: Signal -> HW Signal
sig inputSignal = do
    n <- get
    put $ n + 1
    tell $ mempty {smSignals = [(n, inputSignal)]}
    return $ SigRef n inputSignal
sigp :: Signal -> PipeM Signal
sigp s = lift $ sig s

mkReg :: [(Signal, Signal)] -> HW Signal
mkReg reginput = do
    n <- get
    put $ n + 1
    tell $ mempty {smRegs = [(n, Reg reginput)]}
    return $ RegRef n $ Reg reginput

data LogicStage = LogicStage { lsCtrl :: PipeStageLogic
                             , lsSignal :: Signal
                             , lsReg :: Signal }

stageControl input vld downstreamStages = r where
    or' = MultyOp Or
    and' = MultyOp And
    not' = UnaryOp Not

    upstreamStages = queryUpstreamLStages input
    rdy = or' $ map (pslRdy . lsCtrl) $ upstreamStages
    dep' = and' $ map (pslDe . lsCtrl) $ upstreamStages
    drp = not' vld
    take' = and' [rdy, dep', not' drp]
    takenext' = and' $ map (pslTake . lsCtrl) $ downstreamStages
    dropnext' = and' $ map (pslDrop . lsCtrl) $ downstreamStages
    declr = and' [not' take', (or' [takenext', dropnext'])]
    clr = and' [not' take', (or' [takenext', dropnext'])]

    ctrl deregref = PipeStageLogic
        { pslRdy = rdy
        , pslDep = dep'
        , pslDe  = deregref
        , pslRdyn = and' $ map (pslRdy . lsCtrl) $ downstreamStages
        , pslTake = take'
        , pslTakeNext = takenext'
        , pslDrop = drp
        , pslDropNext = dropnext'
        , pslClr = clr }

    r = do
        dereg <- mkReg [(take', Lit 1 1), (declr, Lit 0 1)]
        reg <- mkReg [(take', input), (clr, Undef)]
        return $ Stage $ LogicStage (ctrl dereg) input reg

stage :: Signal -> PipeM Signal
stage = stage' 0
stage' :: Int -> Signal -> PipeM Signal
stage' bufferdepth inputSignal = do
    np <- get
    pipectrl <- ask

    let
        -- take care of conditional signal
        (vld, inputSignal) = case inputSignal of
            (Cond v s) -> (v, s)
            _ -> (Lit 1 1, inputSignal)

        stgs = pipeCtrlStages pipectrl

        ndelays = maximum $ map (downstreamDist np) downstreamStages
 
        upstreamStages = queryUpstreamStages inputSignal

        stageid = case map pipeStageStageNum $ upstreamStages of
            [] -> 0
            x -> 1 + (maximum x)

        allStagesInPipeline = map snd stgs
        
        hasMeUpstream s = elem np $ map pipeStageId $ pipeStageUpstreamStages s
        downstreamStages = filter hasMeUpstream allStagesInPipeline
       
        name = "stg_" ++ (show np)

        -- enable signal stageid
        rdySignal = Lit 1 1

    let f _ s = stageControl s rdySignal []
    ls <- lift $ foldMapM f [0..ndelays] inputSignal

    let
        self = PipelineStage stg
        stg = PStage    { pipeStageId = np
                        , pipeStageSignal = inputSignal
                        , pipeStageStageNum = stageid
                        , pipeStageUpstreamStages = upstreamStages
                        , pipeStageDownstreamStages = downstreamStages
                        , pipeStageRdy = rdySignal
                        , pipeStageName = name
                        , pipeStageDelaysNum = ndelays
                        , pipeStageBufferDepth = bufferdepth
                        , pipeStageLogicStages = ls }

    return self

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
