#load "Include.fsx"

open Sylvester
open PropCalculus

let p,q,r = var3<bool>

let ``3.31`` = proof prop_calculus <@ (p ||| (q ||| r)) = ((p ||| q) ||| (p ||| r)) @> [
    id_ax prop_calculus <@ p ||| p = p @> |> Transpose |> L
    LeftAssoc |> L
    LeftAssoc |> R
    ident prop_calculus <@ (p ||| p ||| q) = (p ||| q ||| p) @> [R RightAssoc; id_ax prop_calculus <@ (q ||| p) = (p ||| q) @> |> R] |> L
]
let ``3.36`` = proof prop_calculus <@(p |&| q) = (q |&| p) @> [
    LR GoldenRule
    CommuteOr <@ q @> <@ p @> |> R
    CommuteEq <@ q @> <@ p @> |> R 
]

