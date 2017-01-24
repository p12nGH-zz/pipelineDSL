## Description
A Haskell DSL for describing hardware pipelines.

## Example #1
A small example with a 5 stage pipeline (stages named d0-d5) and interfacing with Verilog code(using Alias data constructor).
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

Automatically generated model for the pipeline above in SMT-LIBv2 format:
```SMT
(declare-const data1 (_ BitVec 32))
(declare-const ps_0 (_ BitVec 32))
(assert (= ps_0 data1))
(declare-const ps_1 (_ BitVec 32))
(assert (= ps_1 (bvadd (bvadd ps_0 (_ bv7 32)) ps_0)))
(declare-const ps_2 (_ BitVec 32))
(assert (= ps_2 (bvadd ps_1 (_ bv19 32))))
(declare-const ps_3 (_ BitVec 32))
(assert (= ps_3 (bvadd (bvsub ps_0 (_ bv13 32)) ps_2)))
(declare-const ps_4 (_ BitVec 32))
(assert (= ps_4 ps_3))
(declare-const ps_5 (_ BitVec 32))
(assert (= ps_5 ps_4))
```
