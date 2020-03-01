#load "Include.fsx"

open Sylph
open Operators

let A = F(fun p q -> p = q = q = p)
let C = F (fun p q r -> p |&| q |&| r)

open BooleanAlgebra
let p1 = proof (taut A) boolean_algebra [
    RightAssoc |> EntireA
]

let B = F(fun p q -> not p = q = p = not q)

let p2 = 
        proof (taut B) boolean_algebra [
        LeftA Collect
        EntireA RightAssoc
        RightA Commute
        RightA Collect
        RightA Commute
    ]