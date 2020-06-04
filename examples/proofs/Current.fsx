#load "Include.fsx"

open Sylvester
open PropCalculus
open PredCalculus

let p,q,r,s = var4<bool>
let x,y = var2<bool> 
let p',q',r',s' = <@ p @>, <@ q @>, <@ r @>, <@ s @>
let x' = <@ x @>
let y' = <@ y @>

let P,Q, N, A= var4<bool>
let P',Q', N' = <@ P @>, <@ Q @>, <@ N @>

proof pred_calculus <@ forall x (Q ||| N) P ==> (forall x Q P) @> [
    split_range_forall |> L
    strenghten_and <@ (forall x Q P) @> <@ (forall x N P) @> |> Lemma
]
