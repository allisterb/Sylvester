#load "Include.fsx"
open Sylph

// Define some integer formulae of interest
let F1 = F (fun x -> 2 * x + 8 = 2 * x + 3 + 5)

let F2 = F (fun x -> 2 * x + 3 + 5)
let F3 = F (fun x -> 3 * x + 6 + 2 * x + 4)
let F4 = F (fun x -> 5 * x + 10)

open IntegerAlgebra
let p1 = ident F1 integer_algebra [
    RightAssoc |> EntireB 
    Reduce |> EntireB
]
//F3.Expr
let p2 = theorem (F3 == F4) integer_algebra [ 
    RightAssoc |> EntireA
    Commute |> RightA
    LeftAssoc |> EntireA
    Commute |> EntireA
    LeftAssoc |> EntireA
    LeftAssoc |> EntireA
    Collect |> LeftA
    Reduce |> EntireA
    RightAssoc |> EntireA
    Reduce |> RightA
    Commute |> LeftA
]   

