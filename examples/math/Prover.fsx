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

let p = proof (F1 <=> F2) integer_arithmetic [right_assoc_b; equal_constants_a_b]
p |- (F1 <=> F2)
let j = theorem (F1 <=> F2) p 

let F5 = F(fun x -> 2 * x + 4)
let p5 = axiomatic' F5.Body (commute F5.Body) integer_arithmetic 
  
let p6 = proof (F3 <=> F4) integer_arithmetic [
    right_assoc_a 
    commute_a_right
    right_assoc_a 
    left_assoc_a_right
    equal_constants_a_b
    commute_a_right
    left_assoc_a
    collect_a_left
    equal_constants_a_b
    commute_a_left
    ]
p6 |- (F3 <=> F4)

let (l, r, msg) = p6.State.[7] in (l, r, msg)
