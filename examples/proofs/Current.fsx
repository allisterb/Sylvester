#load "Include.fsx"

open Sylvester
open PropCalculus

/// true = (p = p)
let p,q,r = var3<bool>

(*
let ``3.44`` = proof prop_calculus <@ (p |&| (not p ||| q)) = (p |&| q) @> [
    Commute |> LR
    GoldenRule |> R
    distrib_or <@ p @> <@ not p @> <@ r @> |> R
]

*)
//// p ||| (q ||| r) = (p ||| q) ||| (p ||| r)
proof prop_calculus <@ (p ||| (q ||| r)) = ((p ||| q) ||| (p ||| r)) @> [
    idemp_or <@ p @> |> Trn |> L
    RightAssoc |> R
    LeftAssoc |> L
    LeftAssoc |> R
    commute_or <@ p @> <@ q @> |> L
    //ident prop_calculus <@ (p ||| p ||| q) = (p ||| q ||| p) @> [
    //    R RightAssoc; 
    //    commute_or <@ q @> <@ p @> |> R
    //] |> L
] |> left_state |> expand_left |> src






