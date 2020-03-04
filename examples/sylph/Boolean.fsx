#load "Include.fsx"

open Sylph
open Operators

let A = F (fun (p:bool) q -> (p = q) = (q = p))
let C = F (fun p q r -> p |&| q |&| not r)
//let E = F (fun x -> x + 2)
open BooleanAlgebra
let p1 = taut A boolean_algebra  [
    RightAssoc |> EntireA
    ] 

let D = F (fun (p:bool)   -> p ||| true)
let id1 = ident' (F (fun (p:bool) -> true = (p = p))) []
//
let t3 = taut D S [
    Lemma id1 |> RightA
    Distrib |> EntireA
]


//let y x = x > 5 = true 
//y.GetType().