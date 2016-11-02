import Control.Monad
import Control.Applicative
import Data.List (intercalate) -- nub function removes duplicates
import Data.Monoid ( (<>) )
import Control.Monad.Fix
import Data.Ix (range)

import Hardware.PipelineDSL

t2 :: PipeM ()
t2 = do
    m <- sigp $ Alias "data1" 32
    u <- sigp 899
    d <- stagen "d" $ pPort (Alias "data1en" 1) (Alias "data1" 32)
    d1 <- stagen "d1" $ d + 7 + d -- 1
    d2 <- stage $ d1 + 19 -- 2
    d3 <- stage $ d - 13 + d2 -- 3
    return ()

-- how to run icarus verilog
-- iverilog x.v -g2005-sv -o x && vvp x

main = do
    -- putStr $ toSMT_LIBv2 t2
    putStr $ toVerilog t2
