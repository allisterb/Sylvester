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

let p2 = proof (taut B) S [
        LeftA Collect
        EntireA RightAssoc
        RightA Commute
        RightA Collect
        RightA Commute
    ]

let D = F(fun (p:bool) -> p ||| true)
let a1 = <@ fun (p:bool) -> true = (p = p) @> |> lemma' [Commute' |> RightA]

let p3 = proof (taut D) S [
    a1 |> Subst |> RightA
    Distrib |> EntireA
]