#load "Include.fsx"

open Sylvester
open PropCalculus

// Declare some variables we can reuse in formulae
let p,q,r = var3<bool>

<@ not p = q = p = not q @> |> expand_left |> src

let id1 = theorem prop_calculus <@ (p = (q = q)) = p @>  [
    L LeftAssoc 
    LR RightAssoc
]

let p1 = proof prop_calculus <@ p = p = q = q @>  [
    LR RightAssoc
    LR RightAssoc
    Ident(id1) |> R
]

//id1.Proof.Expr |> expand_left |> src