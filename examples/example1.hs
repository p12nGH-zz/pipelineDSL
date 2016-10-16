import Control.Monad
import Control.Applicative
import Data.List (intercalate) -- nub function removes duplicates
import Data.Monoid ( (<>) )
import Control.Monad.Fix
import Data.Ix (range)

import Hardware.PipelineDSL

t2 :: HW a ()
t2 = do
    m <- sig $ Alias "data1" 32
    u <- sig 899
    d <- stage $ m -- 0
    d1 <- stage $ d + 7 + d -- 1
    d2 <- stage $ d1 + 19 -- 2

    d3 <- stage $ d - 13 + d2 -- 3
    return ()

-- how to run icarus verilog
-- iverilog x.v -g2005-sv -o x && vvp x

main = do
    putStrLn $ "module test;\n" ++
        "logic rst_n;\n" ++
        "logic clk = 0;\n" ++

        "always #10 clk =~ clk;\n" ++
        "initial #14 rst_n = 1;\n" ++

        "initial\n" ++
        "begin\n" ++
        "    rst_n <= 0;\n" ++
        "    $dumpfile(\"test.vcd\");\n" ++
        "    $dumpvars(0,test);\n" ++
        "end\n" ++
        "`include \"tb.sv\" \n"

    putStr $ printRegs t2
    putStrLn "endmodule"
