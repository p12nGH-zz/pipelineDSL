-- | Verilog code generation
module Hardware.PipelineDSL.Verilog (
    toVerilog
) where

import Data.List (intercalate)

import Hardware.PipelineDSL.Pipeline

mOpsSign Or = " | "
mOpsSign And = " & "
mOpsSign Sum = " + "
mOpsSign Mul = " * "

bOpsSign Sub = " - "
bOpsSign Equal = " == "
bOpsSign NotEqual = "!="

uOpsSign Not = "~"
uOpsSign Neg = "-"

vcode :: Signal -> String
vcode = vcode' . simplify . simplify . simplify . simplify  where
    vcode' (SigRef n _) = "sig_" ++ (show n)
    vcode' (MultyOp o ops) = "(" ++ intercalate (mOpsSign o) (map vcode' ops) ++ ")"
    vcode' (BinaryOp o op1 op2) = "(" ++ (vcode' op1) ++ (bOpsSign o) ++ (vcode' op2) ++ ")"

    vcode' (UnaryOp o op@(Alias _ _)) = (uOpsSign o)  ++ (vcode' op)
    vcode' (UnaryOp o op@(SigRef _ _)) = (uOpsSign o)  ++ (vcode' op)
    vcode' (UnaryOp o op@(RegRef _ _)) = (uOpsSign o)  ++ (vcode' op)
    vcode' (UnaryOp o op) = (uOpsSign o) ++ "(" ++ (vcode' op) ++ ")"

    vcode' (Lit val width) = (show width) ++ "'d" ++ (show val)
    vcode' (Alias n _) = n
    vcode' Undef = "'x"
    vcode' (RegRef n _) = "reg_" ++ (show n)

print_width 1 = ""
print_width n = "[" ++ (show $ n - 1) ++ ":0] "

printSigs m = unlines (map printStg stgs) where
    printStg (i, x) = intercalate "\n" [decl] where
        width = getSignalWidth x
        sig = "sig_" ++ (show i)
        decl' = "\n\nlogic " ++ (print_width width) ++ sig ++ ";\n" 
        assign = "assign " ++ sig ++ " = " ++ vcode x ++ ";"
        decl = decl' ++ assign
    (_, s, _) = rPipe m
    stgs = smSignals s

toVerilog m = (printSigs m) ++ (unlines $ map printStg stgs)  where
    printStg (i, x@(Reg c)) = intercalate "\n" [decl] where
        width = maximum $ map (getSignalWidth . snd) c

        reg = "reg_" ++ (show i)
        cond (e, v) =
            "if (" ++ (vcode e) ++ ")\n" ++
            "            " ++ reg ++ " <= " ++ (vcode v) ++ ";"
        condassigns = intercalate "\n        else " $ map cond c

        decl = "\nlogic " ++ (print_width width) ++ reg ++ ";\n" ++
            "always @(posedge clk or negedge rst_n) begin\n" ++
            "    if (rst_n == 0) begin\n" ++
            "        " ++ reg ++ " <= '0;\n" ++
            "    end else begin\n        " ++ condassigns ++
            "\n    end" ++  "\nend"
    (_, s, _) = rPipe m
    stgs = smRegs s
