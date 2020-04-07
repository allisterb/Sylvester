#load "Include.fsx"

open Sylvester
open PropCalculus

let p,q,r = var3<bool>
let p11 = proof prop_calculus <@p |&| q = q |&| p @> [
    L GoldenRule
]