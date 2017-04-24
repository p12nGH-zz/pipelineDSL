module Hardware.PipelineDSL.FSM (
    fsm,
    wait,
    (.=),
    goto,
    task,
    call,
    label
) where

import Control.Monad
import Control.Applicative
import Data.Monoid
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
type FSMM a = RWST (FSM a) (FSM a) (FSMContext, FSMTaskId) (HW a)

data FSMContext = FSMContext Int Int deriving (Eq)
-- create new conditional FSM context, executes on the same cycle as parent context
newFSMContextCond :: FSMContext -> FSMContext
newFSMContextCond (FSMContext s c) = FSMContext s $ c + 1

-- new sequential FSM context, gets enabled at least one clock cycle after the parent context
newFSMContextSeq :: FSMContext -> FSMContext
newFSMContextSeq (FSMContext s c) = FSMContext (s + 1) c

newtype FSMTaskId = FSMTaskId Int deriving (Eq)
newFSMTaskId :: FSMTaskId -> FSMTaskId
newFSMTaskId (FSMTaskId n) = FSMTaskId $ n + 1

data FSMTask = FSMTask FSMTaskId FSMContext

data FSM a = FSM {
    callSites :: [(FSMContext, FSMTaskId)],
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
    enables <- transitions <$> ask
    return $ or' $ map snd $ filter (\(x, _) -> x == i) enables

currentEnable :: FSMM a (Signal a)
currentEnable = fst <$> get >>= contextEnableSignal

bitref s = lift $ (width 1) <$> sig s

fsm :: FSMM a b -> HW a b
fsm f = fst <$> q where
    q = mfix $ \ ~(_, contexts) -> do
        start <- mkRegI [(1, 0)] $ 1
        let
            initial = tell $ mempty {
                transitions = [(FSMContext 0 0, start)]
            }

        (a, _, w) <- (runRWST (initial >> f)) contexts $ (FSMContext 0 0, FSMTaskId 0)

        return (a, w)

wait :: Signal a -> FSMM a FSMContext
wait s = do
    (current_context, current_task) <- get
    current_enable <- contextEnableSignal current_context

    -- create state reg and next context enable logic
    (_, next_enable) <- mfix $ \ ~(r, next) -> do
        r' <- lift $ mkRegI [(current_enable, 1), (next, 0)] $ 0
        next' <- lift $ sig $ and' [r, s]
        return (r', next')

    let next = newFSMContextSeq current_context
    put (next, current_task)
    trns next next_enable
    return next

infixl 2 .=
(.=) r v = do
    (current_context, current_task) <- get
    current_enable <- contextEnableSignal current_context
    lift $ addC r [(current_enable, v)]

    let next = newFSMContextSeq current_context
    trns next current_enable
    put (next, current_task)

    return current_context

trns c e = tell $ mempty {transitions = [(c, e)]}
-- conditional goto
-- branch taken - 1 clock cycle delay
-- not taken - no delay
-- s - destination context
-- c - jump condition

goto :: FSMContext -> Signal a -> FSMM a FSMContext
goto dst c = do
    (current_context, current_task) <- get
    current_enable <- contextEnableSignal current_context

    c' <- lift $ sig c
    dst_enable <- mfix $ \r -> do
        lift $ mkRegI [(and' [current_enable, c'], 1), (r, 0)] 0

    let
        next_enable = and' [current_enable, not' c]
        next = newFSMContextCond current_context

    trns next next_enable
    trns dst  dst_enable

    put (next, current_task)

    return next

-- creates new task in FSM context and returns a handle
task :: FSMM a b -> FSMM a FSMTask
task t = do
    (current_context, current_task) <- get
    before_task_en <- contextEnableSignal current_context

    -- get call sites for this task
    call_contexts <- callSites <$> ask
    fsm_contexts <- transitions <$> ask
    
    -- find enable signals for every context where this task is called
    let
        this_task = newFSMTaskId current_task

        -- task call enable signal
        -- get enable signal for ith context
        enables i = map snd $ filter (\(x, _) -> x == i) fsm_contexts

        f (_, t) = current_task == t
        en = or' $ concat $ map enables $ map fst $ filter f call_contexts
        -- create new context and instantiate task body
        -- add enables to newly created context
        call_context = newFSMContextSeq current_context

    en_sig <- bitref en

    trns call_context en_sig

    put (call_context, this_task) -- instantiate task body in this context

    t

    (current_context, _) <- get
    -- add new context after task body instantiation
    -- propagate enable signal from the context before task body instantiation
    let 
        next = newFSMContextCond current_context

    trns next before_task_en
    put (next, this_task)

    return $ FSMTask current_task current_context


call :: FSMTask -> FSMM a FSMContext
call (FSMTask taskid returnContext) = do
    (current_context, current_task) <- get
    tell $ mempty { callSites = [(current_context, taskid)] }

    thisEn <- contextEnableSignal current_context
    returnEn <- contextEnableSignal returnContext

    -- two possible options
    -- task returns immediately
    -- we need to wait
    wait_reg <- mfix $ \r -> do
        let
            -- wait_return = and' [not' returnEn, thisEn]
            wait_return = thisEn -- will not work if the task returns immediately
            clr_wait = and' [r, returnEn]
        lift $ mkRegI [(wait_return, 1), (clr_wait, 0)] 0
    
    -- after wait context
    let
        returned_immediately = and' [returnEn, thisEn]
        returned_after_wait = and' [returnEn, wait_reg]

        next_en = or' [returned_immediately, returned_after_wait]
        
        next = newFSMContextCond current_context
    
    next_en_s <- lift $ sig $ and' [returnEn, wait_reg]

    tell $ mempty { transitions = [(next, next_en_s)] } 
    put (next, current_task)

    return current_context

label :: FSMM a FSMContext
label = do
    (current_context, current_task) <- get
    current_enable <- contextEnableSignal current_context

    let next = newFSMContextSeq current_context
    trns next current_enable
    put (next, current_task)

    return current_context