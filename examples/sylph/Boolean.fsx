#load "Include.fsx"

open Sylph

let A = F (fun (p) (q) -> p && q || not p && p = q)

let B = F (||)

//A <=> B
//let C = F(fun(p,q) -> p = q)
//let P = (fun () -> A.Apply 6)
//A.Expr

//let pp = prop(3 > 6)

//pp.Expr
open IntegerArithmetic



let gg = F (fun x -> 3 * x)
let hh = F (fun x -> 3 * x)
integer_arithmetic |- gg == hh