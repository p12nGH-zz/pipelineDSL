## Description
A Haskell DSL for describing hardware pipelines.

## Example
<<<<<<< HEAD
A small example with a 5 stage pipeline (stages named d0-d5) and interfacing with Verilog code. 
=======
A small example with a 5 stage pipeline (stages named d0-d5) and interfacing with Verilog code(using Alias data constructor).
>>>>>>> 556b46ba0fd23f6b58a088522b6cde4b8df12783
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
d5 stage has an external enable signal("en"). When this signal is low all the upstream stages are forced to hold their data until d5 enable goes high.
![pipe1](https://cloud.githubusercontent.com/assets/1516471/21633857/a9b7268e-d207-11e6-8d12-69d7521d5db4.png)
