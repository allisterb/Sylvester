#load "Include.fsx"

open Sylph

let A = F (fun (p, q) -> (not p) = q)

let B = F(fun(p,q) -> p = not q)

