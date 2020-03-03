#load "Include.fsx"

open Sylph
open Operators

let A = F (fun p q -> p = q = q = p)
let C = F (fun p q r -> p |&| q |&| r)

open BooleanAlgebra
let p1 = taut A boolean_algebra [
    RightAssoc |> EntireA
]


let D = F (fun p -> p ||| true)
let id1 = ident' (F (fun (p:bool) -> true = (p = p))) [Commute' |> RightA]

let p3 = taut' D  [
    Lemma id1 |> RightA
    Distrib |> EntireA
]

//let t = theorem (Tautology D) p3