#load "Include.fsx"

open Sylvester
open PropCalculus



let p,q,r = var3<bool>
/// not p = q = p = not q
let GoldenRule1 p q = ident prop_calculus <@ (%p |&| %q) = ((%p = %q) = (%p ||| %q)) @> []

GoldenRule1 p q
let p11 = proof prop_calculus <@(p |&| q) = (q |&| p) @> [
    LR GoldenRule
    L Commute
    R Commute
    LR Distrib
    LR Distrib
    //L Distrib
    //R Distrib
    //R Distrib
    //True <@ p @> |> Transpose |> LR
]