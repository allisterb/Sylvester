#load "Include.fsx"

open Sylvester
open PropCalculus

/// true = (p = p)
let p,q,r = var3<bool>

proof prop_calculus <@ (p |&| q |&| r) =  (p = q = r = (p ||| q) = (q ||| r) = (r ||| p) = (p ||| q ||| r)) @> [
    golden_rule <@ p @> <@ q @> |> L
    golden_rule <@ (p = q) = (p ||| q) @> <@ r @> |> L 
    commute_or <@ ((p = q) = (p ||| q)) @> <@ r @> |> L
    distrib_or_eq <@ r @> <@ p = q @> <@ p ||| q @> |> L
    distrib_or_eq <@ r @> <@ p @> <@ q @> |> L
    right_assoc_eq <@ p = q@> <@ p ||| q@> <@ r @> |> L
    commute_eq <@ p ||| q @> <@ r @> |> L
    commute_or <@ r @> <@ q @> |> L
    commute_eq <@ r ||| p@> <@ q ||| r@> |> L
    commute_or <@ r @> <@ p ||| q @> |> L
    L LeftAssoc
    //left_assoc_eq <@ p = q @> <@ r @> <@ (p ||| q) @> |> L
    L LeftAssoc
    L LeftAssoc
    //GoldenRule |> R 
    //GoldenRule |> R
] |> left_src
(*
let ``3.44`` = proof prop_calculus <@ (p |&| (not p ||| q)) = (p |&| q) @> [
    GoldenRule |> L
    Commute |> LR
    distrib_or <@ p @> <@ not p @> <@ q @> |> R
    ExcludedMiddle |> R
    //commute_eq <@ p @> <@not p ||| q @> |> R
    //zero_or <@p ||| q @> |> TrnL |> R
    ident_or_or_not <@ not p @> <@ q @> |> R
    //zero_or <@ p ||| q @> |> Trn |> R
    //left_assoc_or <@ p @> <@ not p @> <@ q @> |> R
    //ExcludedMiddle |> R
    //commute_or <@true @> <@ q @> |> R
    //zero_or <@ q @> |> R
    
    //collect_not <@ p @> <@ q @> |> L
    //Commute |> LR
    //GoldenRule |> R
    //distrib_or <@ p @> <@ not p @> <@ q @> |> R
    //ExcludedMiddle |> R
]
*)

(*
proof prop_calculus <@ (p = q) = ((p |&| q) = (p ||| q)) @> [
    GoldenRule |> R
    //LeftAssoc |> LR
    //LeftAssoc |> LR
    //LeftAssoc |> LR
    //RightAssoc |> LR
    //def_true <@ p = q @> |> L
]
*)





