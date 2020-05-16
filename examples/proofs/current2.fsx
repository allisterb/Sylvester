#load "Include.fsx"

open Sylvester
open PropCalculus

let p,q,r,s = var4<bool> 
let p',q',r',s' = <@ p @>, <@ q @>, <@ r @>, <@ s @>

proof prop_calculus <@(p = q) = (p |&| q) ||| (not p |&| not q)@> [
    right_assoc |> LR
    ident_or_or_not <@ p |&| q@> <@ not p |&| not q@> |> R
] |> left_state |> src