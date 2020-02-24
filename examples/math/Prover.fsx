#load "MathInclude.fsx"

open Sylph
open Sylvester
open IntegerArithmetic

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
let F3 = F h
let F4 = F i 
let F6 = F j
let p = proof (F1 <=> F2) integer_arithmetic [right_assoc_b; equal_constants_a_b]
p |- (F1 <=> F2)
let j = theorem (F1 <=> F2) p 

let F5 = F(fun x -> x * 2 + 4 * x)
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

let p7 = proof (F6 <=> F(fun x -> x *(x + 4))) integer_arithmetic [
    commute_a_right
    //collect_a
    ]
let p8 = proof (F3 <=> F4) integer_arithmetic [
    right_assoc_a 
    commute_a_right
    right_assoc_a 
    left_assoc_a_right
    equal_constants_a_b
]

let p9 = proof (F(fun x -> 3 * x + (10 + 2 * x)) <=> F4) integer_arithmetic [
    commute_a_right
    left_assoc_a
    collect_a_left
    equal_constants_a_b
    commute_a_left
]

let p10 = p8 + p9

let F11 = F (fun (x:int) -> seq {0..x} |> Seq.sum)
F11.Expr