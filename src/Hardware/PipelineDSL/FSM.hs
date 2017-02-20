module Hardware.PipelineDSL.FSM (
    fsm,
    wait,
    (.=)
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
type FSMM a = RWST [(Int, Signal a)] [(Int, Signal a)] Int (HW a)

or' = MultyOp Or
and' = MultyOp And

contextEnableSignal :: Int -> FSMM a (Signal a)
contextEnableSignal i = do
    enables <- ask
    return $ or' $ map snd $ filter (\(x, _) -> x == i) enables

fsm :: FSMM a b -> HW a b
fsm f = fst <$> q where
    q = mfix $ \ ~(_, contexts) -> do
        start <- mkRegI [(Lit 1 1, Lit 0 1)] $ Lit 1 1
        let initial = tell [(0, start)]
        (a, _, w) <- (runRWST (initial >> f)) contexts 0

        return (a, w)

wait :: Signal a -> FSMM a Int
wait s = do
    current_context <- get
    contexts <- ask

    current_enable <- contextEnableSignal current_context

    -- create state reg and next context enable logic
    (_, next_enable) <- mfix $ \ ~(r, next) -> do
        r' <- lift $ mkRegI [(current_enable, Lit 1 1), (next, Lit 0 1)] $ Lit 0 1
        next' <- lift $ sig $ and' [r, s]
        return (r', next')

    let next = current_context + 1
    put next
    tell [(next, next_enable)]
    return next

infixl 2 .=
(.=) r v = do
    current_context <- get
    current_enable <- contextEnableSignal current_context
    lift $ addC r [(current_enable, v)]
    return ()
