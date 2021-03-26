#load "IncludeProver.fsx"

open Sylvester
open PropCalculus


let p,q,r,s = var4<bool>
let p',q',r',s' = <@ p @>, <@ q @>, <@ r @>, <@ s @>
let P,N,A,S = var4<bool>
 
do Proof.LogLevel <- 1
let ``3.52`` = theorem prop_calculus <@ (p = q) = (p |&| q) ||| (not p |&| not q) @> [
        collect |> R
        commute |> L |> L'
        commute |> L
        commute |> R |> L'
        golden_rule' p' q' |> LeftAssoc |> L
]