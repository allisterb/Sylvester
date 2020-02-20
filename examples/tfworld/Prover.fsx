#load "MathInclude.fsx"

open Sylvester
open Sylvester.Prover.IntegerArithmetic

[<ReflectedDefinition;AutoOpen>]
module Formulae =
    let f x = 2 * x + 8 
    let g x = 2 * x + 3 + 5    
    let h x = 3 * x + 6 + 2 * x + 4
    let i x = 5 * x + 10    
    let sum n = Seq.sum (seq {1..n})
    let sum1 n = Seq.sum (seq {1..n + 1})

let V = value 6

let F1, f1 = F(f).Members
let F2, f2 = F(g).Members
let F3, f3 = F(h).Members
let F4, f4 = F(i).Members

let p = Proof(IntegerArithmetic, F1.Expr, F2.Expr, [right_assoc_b; reduce_constants_a_b])
p |- (F1 <=> F2)
let j = thm (F1 <=> F2) p 

let F5 = F(fun x -> 3 * x + 6).Expr |> body

let p5 = axiomatic IntegerArithmetic (F5) (commute F5)

let p6 = proof IntegerArithmetic F3.Expr F4.Expr [left_assoc_a; subst_a p5]
