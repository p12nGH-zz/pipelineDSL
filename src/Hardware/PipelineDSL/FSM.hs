module Hardware.PipelineDSL.FSM (
    fsm,
    wait,
    (.=),
    goto
) where

import Control.Monad
import Control.Applicative
import Data.Monoid ( (<>) )
import Control.Monad.Fix
import Data.Ix (range)
import Control.Monad.Trans.RWS.Lazy hiding (Sum)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (fromMaybe, catMaybes)
import Hardware.PipelineDSL.HW hiding (Signal)
import qualified Hardware.PipelineDSL.HW as HW
import Hardware.PipelineDSL.Verilog

data FSMState a = FSMState 
    { fsmStId :: Int
    , fsmStActivate :: Signal a
    , fsmStActive :: Signal a
    , fsmStPrevious :: [FSMState a]
    , fsmStNext :: [(Signal a, FSMState a)] }

type Signal a = HW.Signal a

-- the FSM monad
type FSMM a = RWST [(FSMContext, Signal a)] [(FSMContext, Signal a)] FSMContext (HW a)

newtype FSMContext = FSMContext Int deriving (Eq)
newFSMContext :: FSMContext -> FSMContext
newFSMContext (FSMContext n) = FSMContext $ n + 1

newtype FSMTask = FSMTask Int
newFSMTask :: FSMTask -> FSMTask
newFSMTask (FSMTask n) = FSMTask $ n + 1

data FSM a = FSM {
    callSites :: [(FSMContext, FSMTask)] ,
    transitions :: [(FSMContext, Signal a)]
}

instance Monoid (FSM a) where
    mempty = FSM [] []
    mappend (FSM c1 t1) (FSM c2 t2) = FSM (c1 <> c2) (t1 <> t2)

or' = MultyOp Or
and' = MultyOp And
not' = UnaryOp Not

contextEnableSignal :: FSMContext -> FSMM a (Signal a)
contextEnableSignal i = do
    enables <- ask
    return $ or' $ map snd $ filter (\(x, _) -> x == i) enables

currentEnable :: FSMM a (Signal a)
currentEnable = get >>= contextEnableSignal

fsm :: FSMM a b -> HW a b
fsm f = fst <$> q where
    q = mfix $ \ ~(_, contexts) -> do
        start <- mkRegI [(Lit 1 1, Lit 0 1)] $ Lit 1 1
        let initial = tell [(FSMContext 0, start)]
        (a, _, w) <- (runRWST (initial >> f)) contexts $ FSMContext 0

        return (a, w)

wait :: Signal a -> FSMM a FSMContext
wait s = do
    current_context <- get
    contexts <- ask

    current_enable <- contextEnableSignal current_context

    -- create state reg and next context enable logic
    (_, next_enable) <- mfix $ \ ~(r, next) -> do
        r' <- lift $ mkRegI [(current_enable, Lit 1 1), (next, Lit 0 1)] $ Lit 0 1
        next' <- lift $ sig $ and' [r, s]
        return (r', next')

    let next = newFSMContext current_context
    put next
    tell [(next, next_enable)]
    return next

infixl 2 .=
(.=) r v = do
    current_context <- get
    current_enable <- contextEnableSignal current_context
    lift $ addC r [(current_enable, v)]
    return ()

-- conditional goto
-- branch taken - 1 clock cycle delay
-- not taken - no delay
goto :: FSMContext -> Signal a -> FSMM a FSMContext
goto s c = do
    current_context <- get
    current_enable <- contextEnableSignal current_context

    dst_enable <- mfix $ \r -> do
        lift $ mkRegI [(and' [current_enable, c], Lit 1 1), (r, Lit 0 1)] $ Lit 0 1
    let
        next_enable = and' [current_enable, not' c]
        next = newFSMContext current_context

    put next

    tell [(next, next_enable)]
    tell [(s, dst_enable)]

    return next