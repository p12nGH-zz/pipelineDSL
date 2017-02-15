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
    , fsmStNext :: [FSMState a] }

type Signal a = HW.Signal a

-- the FSM monad
type FSMM a = RWST [FSMState a] [FSMState a] (FSMState a) (HW a)

or' = MultyOp Or
and' = MultyOp And

fsm :: FSMM a b -> HW a b
fsm f = fst <$> q where
    q = mfix $ \ ~(_, states) -> do
        let
            hasNPrev n s = any ((==) n) $ map fsmStId $ fsmStPrevious s
            nextTo n = filter (hasNPrev n) states

            -- find clear condition for initial state
            initClr = or' $ map fsmStActivate $ nextTo 0

        reg <- mkRegI [(initClr, Lit 0 1)] $ Lit 1 1
        
        let initialState = FSMState
                { fsmStId = 0
                , fsmStActivate = Lit 0 0
                , fsmStActive = reg
                , fsmStPrevious = []
                , fsmStNext = [] }

        (a, _, w) <- (runRWST f) states initialState
        
        return (a, w)

-- record current state in Writer
-- create empty one in State
wait :: Signal a -> FSMM a (FSMState a)
wait s = do
    current_state <- get
    states <- ask

    let
        nextId = 1 + (fsmStId current_state)
        hasNPrev s = elem nextId $ map fsmStId $ fsmStPrevious s
        nextToMe = filter hasNPrev states
        meclr = or' $ map fsmStActivate $ nextToMe

    meset <- lift $ sig $ and' [s, fsmStActive current_state]
    reg <- lift $ mkRegI [(meset, Lit 1 1), (meclr, Lit 0 1)] $ Lit 0 1

    let next = FSMState
            { fsmStId = 1 + (fsmStId current_state)
            , fsmStActivate = meset
            , fsmStActive = reg
            , fsmStPrevious = [current_state]
            , fsmStNext = nextToMe }
    put next
    tell [next]
    return next

infixl 2 .=
(.=) r v = do
    current_state <- get
    lift $ addC r [(fsmStActivate current_state, v)]
    return ()