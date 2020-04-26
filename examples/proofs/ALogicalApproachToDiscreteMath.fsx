#load "Include.fsx"

open Sylvester
open PropCalculus

let p,q,r = var3<bool>

let ``3.31`` = proof prop_calculus <@ (p ||| (q ||| r)) = ((p ||| q) ||| (p ||| r)) @> [
    idemp_or <@ p @> |> Trn |> L
    LeftAssoc |> L
    LeftAssoc |> R
    ident prop_calculus <@ (p ||| p ||| q) = (p ||| q ||| p) @> [R RightAssoc; id_ax prop_calculus <@ (q ||| p) = (p ||| q) @> |> R] |> L
]
let ``3.36`` = proof prop_calculus <@(p |&| q) = (q |&| p) @> [
    LR GoldenRule
    commute_or <@ q @> <@ p @> |> R
    commute_eq <@ q @> <@ p @> |> R 
]

let ``3.43`` = proof prop_calculus <@ (p |&| (p ||| q)) = p @> [
    GoldenRule |> L
    left_assoc_or <@ p @> <@ p @> <@ q @> |> L
    idemp_or <@ p @> |> L
]

let ``3.44`` = proof prop_calculus <@ (p ||| (p |&| q)) = p @> [
    GoldenRule |> L
    Distrib |> L
    Distrib |> R
    left_assoc_or <@ p @> <@ p @> <@ q @> |> L
    idemp_or <@ p @> |> L
    Distrib |> L
    idemp_or <@ p @> |> L
    //left_assoc_or <@ p @> <@ p @> <@ q @> |> L
    //idemp_or <@ p @> |> L
]
