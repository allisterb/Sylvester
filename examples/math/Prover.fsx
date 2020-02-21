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

let F1 = F f
let F2 = F g
let F3 = F h
let F4 = F i 

let p = proof (F1 <=> F2) integer_arithmetic [right_assoc_b; reduce_constants_a_b]
p |- (F1 <=> F2)
let j = theorem (F1 <=> F2) p 

let F5 = F(fun x -> 2 * x + 4)
let p5 = axiomatic' F5.Body (commute F5.Body) integer_arithmetic 
  
let p6 = proof (F3 <=> F4) integer_arithmetic [
    right_assoc_a 
    commute_a_right
    left_assoc_a
    right_assoc_a
    right_assoc_a 
    left_assoc_a_right
    reduce_constants_a_b
    commute_a_right
    left_assoc_a
    ]
p6 |- (F3 <=> F4)
