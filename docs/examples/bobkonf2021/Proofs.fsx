#load "IncludeProver.fsx"

open Sylvester
open PropCalculus
open PredCalculus

let p,q,r,s = var4<bool>
let p',q',r',s' = <@ p @>, <@ q @>, <@ r @>, <@ s @>
let P,N,A,S = var4<bool>
 
let bounded s:seq<'t> = 
        let x = elem s
        let epsilon,L = var<real>
        let n,N = var2<int>
        forall(epsilon, epsilon > 0, exists(N, n > N, x.[n] - L < epsilon))

 let ddd = Define("test", fun e )
do Proof.LogLevel <- 1
let ``3.52`` = theorem prop_calculus <@ (p = q) = (p |&| q) ||| (not p |&| not q) @> [
        collect |> R
        commute |> L |> L'
        commute |> L
        commute |> R |> L'
        golden_rule' p' q' |> LeftAssoc |> L
]