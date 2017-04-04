import Hardware.PipelineDSL

(.==) = BinaryOp (Cmp Equal)
(.!=) = BinaryOp (Cmp NotEqual)

decr x = x .= x - 1

main = putStrLn $ toVerilog $ do
    let s1 = (Alias "sig" 2)
    r <- mkReg []
    p <- mkReg []
    fsm $ do

        wait_some <- task $ do
            r .= 17
            c <- decr r
            goto c $ r .!= 2
            wait $ Lit 1 1

        p .= 3
        call wait_some
        
        p .= 18
        call wait_some
        p .= 31
        -- call wait_some

        return ()
