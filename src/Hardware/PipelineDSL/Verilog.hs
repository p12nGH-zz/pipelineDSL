-- | Verilog code generation
module Hardware.PipelineDSL.Verilog (
    toVerilog,
) where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)

import Hardware.PipelineDSL.HW

mOpsSign Or = " | "
mOpsSign And = " & "
mOpsSign Sum = " + "
mOpsSign Mul = " * "

bOpsSign Sub = " - "
bOpsSign (Cmp Equal) = " == "
bOpsSign (Cmp NotEqual) = " != "
bOpsSign (Cmp LessOrEqual) = " <= "
bOpsSign (Cmp GreaterOrEqual) = " >= "
bOpsSign (Cmp Less) = " < "
bOpsSign (Cmp Greater) = " > "

uOpsSign Not = "~"
uOpsSign Neg = "-"

vcode :: Signal a -> String
vcode = vcode' . simplify . simplify . simplify . simplify  where
    vcode' (SigRef n HWNNoName _) = "sig_" ++ (show n)
    vcode' (SigRef _ (HWNExact n) _) = n
    vcode' (SigRef _ (HWNLike n) _) = n ++ "_" ++ (show n)
    vcode' (MultyOp o ops) = "(" ++ intercalate (mOpsSign o) (map vcode' ops) ++ ")"
    vcode' (BinaryOp o op1 op2) = "(" ++ (vcode' op1) ++ (bOpsSign o) ++ (vcode' op2) ++ ")"

    vcode' (UnaryOp o op@(Alias _ _)) = (uOpsSign o)  ++ (vcode' op)
    vcode' (UnaryOp o op@(SigRef _ _ _)) = (uOpsSign o)  ++ (vcode' op)
    vcode' (UnaryOp o op@(RegRef _ _)) = (uOpsSign o)  ++ (vcode' op)
    vcode' (UnaryOp o op) = (uOpsSign o) ++ "(" ++ (vcode' op) ++ ")"

    vcode' (Lit val width) = (show width) ++ "'d" ++ (show val)
    vcode' (Alias n _) = n
    vcode' (WidthHint _ s) = vcode' s
    vcode' Undef = "'x"
    vcode' (ExtRef _ n) = vcode' n
    vcode' (RegRef n (Reg _ _ Nothing)) = "reg_" ++ (show n)
    vcode' (RegRef n (Reg _ _ (Just name))) = name ++ (show n)

print_width 1 = ""
print_width n = "[" ++ (show $ n - 1) ++ ":0] "

printSigs s = unlines (map printStg stgs) where
    printStg (Comb i x name declare) = intercalate "\n" [decl] where
        width = getSignalWidth (Just i) x
        sig = case name of
            HWNNoName -> "sig_" ++ (show i)
            HWNLike n -> n ++ "_" ++ (show i)
            HWNExact n -> n
        decl' = "\n\nlogic " ++ (print_width width) ++ sig ++ ";\n"
        assign = "assign " ++ sig ++ " = " ++ vcode x ++ ";"
        decl = (if declare then decl' else "\n") ++ assign
    stgs = smSignals s

toVerilog m = toVerilog' s where
    (_, s) = rHW m

toVerilog' s = (printSigs s) ++ (unlines $ map printStg stgs)  where
    stgs = smRegs s
    addnl_conditions n = concat $ map u $ filter (f n) $ smRegCs s where
        f n (RegC m _) = m == n -- filter 
        u (RegC _ c) = c -- unpack
    
    printStg (i, x@(Reg c reset_value mname)) = intercalate "\n" [decl] where
        width = maximum $ map ((getSignalWidth (Just i)). snd) (c ++ (addnl_conditions i))

        name = fromMaybe "reg_" mname
        reg = name ++ (show i)
        cond (e, v) =
            "if (" ++ (vcode e) ++ ")\n" ++
            "            " ++ reg ++ " <= " ++ (vcode v) ++ ";"
        condassigns = intercalate "\n        else " $ map cond (c ++ (addnl_conditions i))

        decl = "\nlogic " ++ (print_width width) ++ reg ++ ";\n" ++
            "always @(posedge clk or negedge rst_n) begin\n" ++
            "    if (rst_n == 0) begin\n" ++
            "        " ++ reg ++ " <= " ++ (vcode reset_value) ++ ";\n" ++
            "    end else begin\n        " ++ condassigns ++
            "\n    end" ++  "\nend"
