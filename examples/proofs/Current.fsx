#load "Include.fsx"

open Sylvester
open PredCalculus

let p,q,r,s = var4<bool> 
let p',q',r',s' = <@ p @>, <@ q @>, <@ r @>, <@ s @>
let i,j,k = var3<int>

[<Formula>]
let p1 = forall (p, q) ((p ||| q) = true)

[<Formula>]
let p2 = forall (p, q) ((r ||| s) = true)

[<Formula>]
let p3 = forall (p, q) (((p ||| q) = true) |&| ((r ||| s) = true))

proof pred_calculus <@  (p1 |&| p2) = p3 @> []