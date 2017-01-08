## Description
A Haskell DSL for describing hardware pipelines.

## Example
A small example with a 5 stage pipeline (stages named d0-d5) and interfacing with Verilog code. 
```haskell
pipeline_1 = do
    m <- sigp $ Alias "data1" 32
    u <- sigp 899
    let
        en = Alias "en" 1
    d <- stagen "d0" $ pPort (Alias "data1en" 1) (Alias "data1" 32)
    d1 <- stagen "d1" $ d + 7 + d -- 1
    d2 <- stagen "d2" $ d1 + 19 -- 2
    d3 <- stagen "d3" $ d - 13 + d2 -- 3
    d4 <- stagen "d4" d3
    d5 <- stageEnN "d5" en d4
```