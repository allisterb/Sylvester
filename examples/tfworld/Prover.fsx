#load "MathInclude.fsx"

open Sylvester
open Sylvester.Prover.Arithmetic

[<ReflectedDefinition;AutoOpen>]
module Formulae =
    let f x = 2 * x + 8 
    let g x = 2 * x + 3 + 5    
    let h x = 3 * x + 6 + 2 * x + 4
    let sum n = Seq.sum (seq {1..n})
    let sum1 n = Seq.sum (seq {1..n + 1})

let V = value 6

let F1, f1 = Formula(f).Members
let F2, f2 = Formula(g).Members
let F3, f3 = Formula(sum).Members

//F3
let p = Proof(Arithmetic, F1.Expr, F2.Expr, [right_assoc_b; reduce_constants_a_b])
(p |- (F1, F2))