import Hardware.PipelineDSL

(.==) = BinaryOp (Cmp Equal)
(.!=) = BinaryOp (Cmp NotEqual)

main = putStrLn $ toVerilog $ do
    let s1 = (Alias "sig" 2)
    r <- mkReg []
    fsm $ do
        wait $ s1 .== 1
        r .= 11
        l <- wait $ s1 .== 2
        r .= r - 1
        goto l $ r .!= 2
        wait $ s1 .== 3
