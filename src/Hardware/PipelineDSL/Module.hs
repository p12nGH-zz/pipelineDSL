module Hardware.PipelineDSL.Module (
    input,
    output,
    verilogM
) where

import Hardware.PipelineDSL.HW
import Hardware.PipelineDSL.Verilog 
import Control.Monad.Writer.Lazy
import Data.List (intercalate)

data Port = InputPort String | OutputPort String
type SignalM = Signal ()
type ModuleM = WriterT [Port] (HW ())

input :: String -> Int -> ModuleM SignalM
input n w = do
    tell $ [InputPort n]
    return (Alias n w)

output :: String -> SignalM -> ModuleM ()
output n s = do
    tell $ [OutputPort n]
    lift $ sigalias' n s
    return ()

verilogM :: String -> ModuleM a -> String
verilogM name m = interface ++ code ++ "endmodule" where
    pp (InputPort s) = "input " ++ s
    pp (OutputPort s) = "output " ++ s
    hw = runWriterT m
    ((_, ports), _) = rHW $ hw
    interface = "module " ++ name ++ "(input logic clk, input logic rst_n, " ++ (intercalate ", " $ map pp ports) ++ ");"
    code = toVerilog hw
