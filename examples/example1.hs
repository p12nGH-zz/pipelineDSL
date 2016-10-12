import Control.Monad
import Control.Applicative
import Data.List (intercalate) -- nub function removes duplicates
import Data.Monoid ( (<>) )
import Control.Monad.Fix
import Data.Ix (range)

import Hardware.Pipeline

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

t1 r r2 = do
    a <- sig $ Alias "data1" 11
    b <- sig $ a + a + a + 43
    t <- sig $ r + 17 + r2
    d <- forM [1, 3, 4] $ \x -> do
        sig a
    l <- sig $ foldr (+) (head d) (tail d)
    c <- sig $ a + b
    return (a, t)

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
