module Hardware.PipelineDSL.Pipeline (
    sig,
    sigp,
    stage,
    stagen,
    HW (..),
    PipeM (..),
    Signal (..),
    PStage (..),
    SigMap (..),
    StgMap (..),
    MOps (..),
    BOps (..),
    UOps (..),
    CmpOp (..),
    Reg (..),
    LogicStage (..),
    simplify,
    getSignalWidth,
    rPipe,
    rHW,
    pPort,
    IPortNB (..),
    stageEn,
    stageEnN
) where

import Control.Monad
import Control.Applicative
import Data.Monoid ( (<>) )
import Control.Monad.Fix
import Data.Ix (range)
import Data.Bits (finiteBitSize, countLeadingZeros)
import Control.Monad.RWS.Lazy hiding (Sum)
import Data.Maybe (fromMaybe)

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
data IPortNB = IPortNB { portEn :: Signal
                   , portData :: Signal }

data CmpOp = Equal | NotEqual | LessOrEqual | GreaterOrEqual | Less | Greater
data MOps = Or | And | Sum | Mul
data BOps = Sub | Cmp CmpOp
data UOps = Not | Neg | Signum | Abs

data Signal = Alias String Int -- name, width
            | Lit Int Int -- toInteger, width can be fixed or any (value, width)
            | SigRef Int (Maybe String) Signal
            | UnaryOp UOps Signal
            | MultyOp MOps [Signal]
            | BinaryOp BOps Signal Signal
            | Cond Signal Signal -- conditional signal valid, value
            | Undef
            | IPipePortNB IPortNB -- enable sig
            | Stage LogicStage
            -- register output. managed separately outside of the HW monad
            | PipelineStage PStage
            | RegRef Int Reg -- register, inserts 1 clock delay

-- list all pipeline stages that are inputs for a given signal

queryUpstreamLStages :: Signal -> [LogicStage]
queryUpstreamLStages (Stage x) = [x]
queryUpstreamLStages (SigRef _ _ s) = queryUpstreamLStages s
queryUpstreamLStages (MultyOp _ s) = concat $ map queryUpstreamLStages s
queryUpstreamLStages (BinaryOp _ s1 s2) = (queryUpstreamLStages s1) ++ (queryUpstreamLStages s2)
queryUpstreamLStages (UnaryOp _ s) = queryUpstreamLStages s
queryUpstreamLStages _ = []

queryIPipePortNBs :: Signal -> [IPortNB]
queryIPipePortNBs (IPipePortNB x) = [x]
queryIPipePortNBs (SigRef _ _ s) = queryIPipePortNBs s
queryIPipePortNBs (MultyOp _ s) = concat $ map queryIPipePortNBs s
queryIPipePortNBs (BinaryOp _ s1 s2) = (queryIPipePortNBs s1) ++ (queryIPipePortNBs s2)
queryIPipePortNBs (UnaryOp _ s) = queryIPipePortNBs s
queryIPipePortNBs _ = []

queryUpstreamStages :: Signal -> [PStage]
queryUpstreamStages (PipelineStage x) = [x]
queryUpstreamStages (SigRef _ _ s) = queryUpstreamStages s
queryUpstreamStages (MultyOp _ s) = concat $ map queryUpstreamStages s
queryUpstreamStages (BinaryOp _ s1 s2) = (queryUpstreamStages s1) ++ (queryUpstreamStages s2)
queryUpstreamStages (UnaryOp _ s) = queryUpstreamStages s
queryUpstreamStages _ = []

-- longest distance to current stage
-- shorter paths need to be compensated
-- returns 0 if there is no paths connecting specifid stage
downstreamDist :: Int -> Signal -> Int
downstreamDist stgid sig = r stgidPaths where
    -- get all stages and distances
    -- returns an array of pairs: [(distance, stage)]
    allStgsDist sig = c ++ n where
        t = queryUpstreamStages sig
        c = map ((,) 0) t
        n = map (\(x, y) -> (x + 1, y)) $ concat $ map (allStgsDist . pipeStageSignal) t
    stgidPaths = filter (\(_, s) -> (pipeStageId s) == stgid) $ allStgsDist sig
    r [] = 0
    r x = maximum $ map fst x

getSignalWidth :: Signal -> Int
getSignalWidth (PipelineStage s) = getSignalWidth $ pipeStageSignal s
getSignalWidth (SigRef _ _ s) = getSignalWidth s
getSignalWidth (MultyOp _ []) = 0 -- never happens
getSignalWidth (MultyOp _ s) = maximum $ map getSignalWidth s
getSignalWidth (BinaryOp (Cmp _) _ _) = 1
getSignalWidth (BinaryOp _ s1 s2) = max (getSignalWidth s1) (getSignalWidth s2)
getSignalWidth (UnaryOp _ s) = getSignalWidth s
getSignalWidth (Cond _ s) = getSignalWidth s
getSignalWidth (Lit _ s) = s
getSignalWidth (Alias _ s) = s
getSignalWidth (RegRef _ (Reg s _ _)) = maximum $ map (getSignalWidth . snd) s
getSignalWidth Undef = 0
getSignalWidth (Stage ls) = getSignalWidth $ lsSignal ls
getSignalWidth (IPipePortNB p) = getSignalWidth $ portData p

mapSignal :: (Signal -> Signal) -> Signal -> Signal
mapSignal f s =  mapSignal' (f s) where
    -- mapSignal aplies the transformation, mapSignal' does structure-preserving traversing
    mapSignal' (MultyOp op s) = MultyOp op $ map (mapSignal f) s
    mapSignal' (BinaryOp op s1 s2) = BinaryOp op (mapSignal f s1) (mapSignal f s2)
    mapSignal' (UnaryOp op s) = UnaryOp op (mapSignal f s)
    mapSignal' (SigRef n name s) = SigRef n name (mapSignal f s)
    mapSignal' (Cond n s) = Cond (mapSignal f n) (mapSignal f s)
    mapSignal' x = x

rewrite :: (Signal -> Signal) -> Signal -> Signal
rewrite f s =  f $ rewrite' (f s) where
    -- rewrite aplies the transformation, rewrite' does structure-preserving traversing
    rewrite' (MultyOp op s) = f $ MultyOp op $ map (rewrite f) s
    rewrite' (BinaryOp op s1 s2) = f $ BinaryOp op (rewrite f s1) (rewrite f s2)
    rewrite' (UnaryOp op s) = f $ UnaryOp op (rewrite f s)
    rewrite' (SigRef n name s) = f $ SigRef n name (rewrite f s)
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
-- condition/value pairs, initial(reset) value, optional name
data Reg = Reg [(Signal, Signal)] Signal (Maybe String)
data SigMap = SigMap { smSignals :: [(Int, Signal, Maybe String)]
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

rHW m = (a, sigs, sm) where
    r@((a, _, sm), _, sigs) = runRWS m () 0

-- creates reference
sig :: Signal -> HW Signal
sig inputSignal = sig' inputSignal Nothing

sign :: String -> Signal -> HW Signal
sign name inputSignal = sig' inputSignal (Just name)

sig' :: Signal -> (Maybe String) -> HW Signal
sig' inputSignal name = do
    n <- get
    put $ n + 1
    tell $ mempty {smSignals = [(n, inputSignal, name)]}
    return $ SigRef n name inputSignal

sigp :: Signal -> PipeM Signal
sigp s = lift $ sig s

mkReg :: [(Signal, Signal)] -> HW Signal
mkReg = mkReg' Nothing 0
mkNReg n = mkReg' (Just n) 0
mkNRegX n = mkReg' (Just n) Undef

pPort :: Signal -> Signal -> Signal
pPort en s = IPipePortNB $ IPortNB {portData = s, portEn = en}

mkReg' :: Maybe String -> Signal -> [(Signal, Signal)]  -> HW Signal
mkReg' name reset_value reginput = do
    n <- get
    put $ n + 1
    let r =  Reg reginput reset_value name
    tell $ mempty {smRegs = [(n, r)]}
    return $ RegRef n r

 

data LogicStage = LogicStage { lsCtrl :: PipeStageLogic
                             , lsSignal :: Signal
                             , lsReg :: Signal }

stageControl name input vld rdy downstreamStages = mfix $ \me -> do
    let
        or' = MultyOp Or
        and' = MultyOp And
        not' = UnaryOp Not

        upstreamStages = queryUpstreamLStages input
        upstreamPorts = queryIPipePortNBs input
        rdy' = and' [rdy, (and' (map (pslRdy . lsCtrl) $ upstreamStages))]
        deUpStgs = map (pslDe . lsCtrl) $ upstreamStages
        deUIPortNBs = map portEn upstreamPorts
        dep' = and' $ deUpStgs ++ deUIPortNBs
        drp = not' vld
        (Stage mestg) = me
        take' = and' [rdy', dep', not' drp, or' [not' $ pslDe $ lsCtrl mestg, takenext']]
        dsLStgs = concat $ map queryUpstreamLStages downstreamStages
        takenext' = and' $ map (pslTake . lsCtrl) $ dsLStgs
        dropnext' = and' $ map (pslDrop . lsCtrl) $ dsLStgs
        declr = and' [not' take', (or' [takenext', dropnext'])]
        clr = and' [not' take', (or' [takenext', dropnext'])]

    dereg <- mkNReg (name ++ "_dereg") [(take', Lit 1 1), (declr, Lit 0 1)]
    reg <- mkNRegX (name ++ "_lstgr") [(take', input), (clr, Undef)]
    sign (name ++ "_take") take'
    sign (name ++ "_rdy") rdy'
    
    let
        ctrl = PipeStageLogic
            { pslRdy = rdy'
            , pslDep = dep'
            , pslDe  = dereg
            , pslRdyn = and' $ map (pslRdy . lsCtrl) $ dsLStgs
            , pslTake = take'
            , pslTakeNext = takenext'
            , pslDrop = drp
            , pslDropNext = dropnext'
            , pslClr = clr }
    return $ Stage $ LogicStage ctrl input reg

stage :: Signal -> PipeM Signal
stage = stage' Nothing (Lit 1 1) 0

stagen :: String -> Signal -> PipeM Signal
stagen name s = stage' (Just name) (Lit 1 1) 0 s

stageEn :: Signal -> Signal -> PipeM Signal
stageEn en s = stage' Nothing en 0 s

stageEnN :: String -> Signal -> Signal -> PipeM Signal
stageEnN name en s = stage' (Just name) en 0 s

stage' :: (Maybe String) -> Signal -> Int -> Signal -> PipeM Signal
stage' mname rdySignal bufferdepth inputSignal' = do
    np <- get
    pipectrl <- ask
    put $ np + 1

    let
        -- take care of conditional signal
        (vld, inputSignal) = case inputSignal' of
            (Cond v s) -> (v, s)
            _ -> (Lit 1 1, inputSignal')

        stgs = pipeCtrlStages pipectrl

        dsDistances = map (downstreamDist np) (map pipeStageSignal downstreamStages)
        ndelays = case dsDistances of
            [] -> 0
            s -> maximum s

        -- get list of downstreamStages at distance d
        pickDsByDistance d = map (head . pipeStageLogicStages . fst) $
            filter (\x -> (snd x) == d) (zip downstreamStages dsDistances) 

        ds = mapSignal mapR inputSignal
        mapR (PipelineStage p) = ls !! dist where
            ls = pipeStageLogicStages p
            dist = downstreamDist (pipeStageId p) inputSignal'
        mapR x = x

        upstreamStages = queryUpstreamStages inputSignal

        stageid = case map pipeStageStageNum $ upstreamStages of
            [] -> 0
            x -> 1 + (maximum x)

        allStagesInPipeline = map snd stgs
        
        hasMeUpstream s = elem np $ map pipeStageId $ pipeStageUpstreamStages s
        downstreamStages = filter hasMeUpstream allStagesInPipeline
       
        name = fromMaybe ("stg_" ++ (show np)) mname
        stageControl' = stageControl name

    let
        f ds s = stageControl' s (Lit 1 1) (Lit 1 1) ds
    r <- lift $ stageControl' ds vld rdySignal (pickDsByDistance 0)
    ls <- lift $ foldMapM f (map pickDsByDistance [1..ndelays]) r

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
                        , pipeStageLogicStages = r:ls }
    tell $ mempty {smStages = [(np, stg)]}
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
