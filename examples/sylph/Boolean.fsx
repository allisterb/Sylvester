#load "Include.fsx"

open Sylph
open Operators

let A = F(fun p q -> p = q = q = p)
let C = F (fun p q r -> p |&| q |&| r)

open BooleanAlgebra
let p1 = proof (A == Prop.T) boolean_algebra [
    RightAssoc |> EntireA
]

A.Expr