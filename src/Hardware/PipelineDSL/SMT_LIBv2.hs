
module Hardware.PipelineDSL.SMT_LIBv2 (
    toSMT_LIBv2
) where

import Data.List (intercalate)
import Text.Printf (printf)

import Hardware.PipelineDSL.Pipeline

mOpsSign Or = "bvor "
mOpsSign And = "bvand "
mOpsSign Sum = "bvadd "
mOpsSign Mul = "bvmul "

bOpsSign Sub = "bvsub "
bOpsSign Equal = "="
bOpsSign NotEqual = "n/s"

uOpsSign Not = "bvnot"
uOpsSign Neg = "n/s"

code :: Signal -> String
code = code' . simplify . simplify . simplify . simplify  where
    code' (SigRef n _) = "sig_" ++ (show n)

    code' (MultyOp o (op:[])) = code op
    code' (MultyOp o (op:ops)) = "(" ++ (mOpsSign o) ++ (code op) ++ " " ++ (code' (MultyOp o ops)) ++  ")"

    code' (BinaryOp o op1 op2) = "(" ++ (bOpsSign o) ++ (code' op1) ++ " " ++ (code' op2) ++ ")"
    code' (UnaryOp o op) = "( " ++ (uOpsSign o) ++ (code' op) ++ ")"
    code' (Lit val width) = printf "#x%08x" val
    code' (Alias n _) = n
    code' Undef = "'x"
    code' (RegRef n _) = "reg_" ++ (show n)
    code' (PipelineStage p) = code' $ pipeStageReg p

toSMT_LIBv2 m = signals ++ stages where
    signals = unlines (map printSig $ smSignals $ rHW m)
    printSig (i, x) = assert where
        width = getSignalWidth x
        sig = "sig_" ++ (show i)
        decl = "(declare-const " ++ sig ++ " (_ BitVec " ++ (show width) ++ "))\n"
        assert = decl ++ "(assert (= " ++ sig ++ " " ++ (code x) ++ "))"
    stages = unlines (map printStg $ smStages $ rHW m)
    printStg (i, pstg) = assert where
        s = pipeStageSignal pstg
        width = getSignalWidth  s
        reg = "reg_" ++ (show i)
        decl = "(declare-const " ++ reg ++ " (_ BitVec " ++ (show width) ++ "))\n"
        assert = decl ++ "(assert (= " ++ reg ++ " " ++ (code s) ++ "))"
