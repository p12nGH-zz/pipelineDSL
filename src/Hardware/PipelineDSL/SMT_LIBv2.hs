
module Hardware.PipelineDSL.SMT_LIBv2 (
    toSMT_LIBv2
) where

import Data.List (intercalate)
import Text.Printf (printf)

import Hardware.PipelineDSL.Pipeline
import Hardware.PipelineDSL.HW

mOpsSign Or = "bvor "
mOpsSign And = "bvand "
mOpsSign Sum = "bvadd "
mOpsSign Mul = "bvmul "

bOpsSign Sub = "bvsub "
bOpsSign (Cmp Equal) = "="
bOpsSign (Cmp NotEqual) = "n/s"

uOpsSign Not = "bvnot"
uOpsSign Neg = "n/s"

code :: Signal a -> String
code = code' . simplify . simplify . simplify . simplify  where
    code' (SigRef n HWNNoName _) = "sig_" ++ (show n)
    code' (SigRef _ (HWNExact n) _) = n
    code' (SigRef _ (HWNLike n) _) = n ++ "_" ++ (show n)

    code' (MultyOp o (op:[])) = code op
    code' (MultyOp o (op:ops)) = "(" ++ (mOpsSign o) ++ (code op) ++ " " ++ (code' (MultyOp o ops)) ++  ")"

    code' (BinaryOp o op1 op2) = "(" ++ (bOpsSign o) ++ (code' op1) ++ " " ++ (code' op2) ++ ")"
    code' (UnaryOp o op) = "( " ++ (uOpsSign o) ++ (code' op) ++ ")"
    code' (Lit val width) = printf "(_ bv%d %d)" val width 
    code' (Alias n _) = n
    code' Undef = "'x"
    code' (RegRef n _) = "reg_" ++ (show n)

toSMT_LIBv2 m = signals ++ stages where
    (_, h, s) = rPipe m
    signals = unlines (map printSig $ smSignals h)
    printSig (i, x, name) = assert where
        width = getSignalWidth (Just i) x
        sig = case name of
            HWNNoName -> "sig_" ++ (show i)
            HWNExact n -> n
            HWNLike n -> n ++ "_" ++ (show i)
        decl = "(declare-const " ++ sig ++ " (_ BitVec " ++ (show width) ++ "))\n"
        assert = decl ++ "(assert (= " ++ sig ++ " " ++ (code x) ++ "))"
    stages = unlines (map printStg $ smStages s)
    printStg (i, pstg) = assert where
        s = pipeStageSignal pstg
        width = getSignalWidth (Just i) s
        reg = "ps_" ++ (show i)
        decl = "(declare-const " ++ reg ++ " (_ BitVec " ++ (show width) ++ "))\n"
        assert = decl ++ "(assert (= " ++ reg ++ " " ++ (code s) ++ "))"
