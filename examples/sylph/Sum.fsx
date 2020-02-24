#load "Include.fsx"

open Sylph

[<ReflectedDefinition;AutoOpen>]
module Formulae =
    let f x = 2 * x + 8 
    let g x = 2 * x + 3 + 5    
    let h x = 3 * x + 6 + 2 * x + 4
    let i x = 5 * x + 10    
    let j x = x * x + 4 * x
    let sum n = Seq.sum (seq {1..n})
    let sum1 n = Seq.sum (seq {1..n + 1})

let F1 = F f
let F2 = F g

src F2.Expr