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

let F1, f1, _ = F(f).Form
let F2, f2, _= F(g).Form
let F3, f3, _ = F(h).Form
let F4, f4, _ = F(i).Form

let p = proof IntegerArithmetic F1 F2 [right_assoc_b; reduce_constants_a_b]
p |- (F1 <=> F2)
let j = theorem (F1 <=> F2) p 

let F5 = F(fun x -> 2 * x + 4)

let p5 = axiomatic' IntegerArithmetic F5.Body (commute F5.Body)
(right_assoc >> subst p5) F3.Expr

//(right_assoc >> right_assoc >> src) F3.Expr  
let p6 = proof IntegerArithmetic F3 F4 [right_assoc_a; commute_a_right;left_assoc_a;right_assoc_a;right_assoc_a; left_assoc_a_right;reduce_constants_a_b]