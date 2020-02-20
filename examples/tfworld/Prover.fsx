#load "MathInclude.fsx"

open Sylvester
open Sylvester.Prover.Arithmetic

[<ReflectedDefinition;AutoOpen>]
module Formulae =
    let f x = 2 * x + 8 
    let g x = 2 * x + 3 + 5    
    let h x = 3 * x + 6 + 2 * x + 4
    let h1 x = 3 * x + 6
    let h2 x = 2 * x + 4 
    let h3 x = h1 x + h2 x
    let sum n = Seq.sum (seq {1..n})
    let sum1 n = Seq.sum (seq {1..n + 1})

let V = value 6

let F1, f1 = F(f).Members
let F2, f2 = F(g).Members
let F3, f3 = F(h).Members
let F4, f4 = F(h1).Members
let F5, f5 = F(h2).Members
let F6, f6 = F(h3).Members
F3
let p = Proof(IntegerArithmetic, F1.Expr, F2.Expr, [right_assoc_b; reduce_constants_a_b])

p |- (F1 <=> F2)

let j = thm (F1 <=> F2) p 

F6.Expr |> left_assoc |> (sequal F3.Expr)

[<ReflectedDefinition>]
let p1 = F (fun x -> x > 0)
[<ReflectedDefinition>]
let p2 = F (fun x -> x > 5)

p2 * p1