import Hardware.PipelineDSL

(.==) = BinaryOp (Cmp Equal)

main = putStrLn $ toVerilog $ do
    let s1 = (Alias "sig" 2)
    fsm $ do
        wait $ s1 .== 1
        wait $ s1 .== 2
        wait $ s1 .== 3
