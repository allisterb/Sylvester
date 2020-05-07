#load "Include.fsx"

open Sylvester
open PropCalculus

/// true = (p = p)
let p,q,r,s = var4<bool>
let p',q',r',s' = <@ p @>, <@ q @>, <@ r @>, <@ s @>

proof prop_calculus <@ (p |&| q |&| r) = (p |&| (q |&| r)) @> [
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
    LeftAssoc |> L
    LeftAssoc |> LL
    LeftAssoc |> LLLL
    //commute_and <@ p @> <@ q |&| r @> |> R
    //ident_and_eq_all <@ q @> <@ r @> <@ p @> |> R
    //commute_eq <@q = r@> <@ p @> |> R
] |> last_state |> expand_left |> expand_left |> expand_left |> expand_left |> src
(*
let lr p q r = id_ax prop_calculus <@ (%p = %q) = %r = (%p = (%q = %r))@>
lr p' q' r'
/// p ||| (q |&| r) = ((p ||| q) |&| (p ||| r))
proof prop_calculus <@ p ||| (q |&| r) = ((p ||| q) |&| (p ||| r)) @> [
        golden_rule <@ q @> <@ r @> |> L
        golden_rule <@ p ||| q @> <@ p ||| r @> |> R
        distrib_or_eq <@ p @> <@ q = r @> <@ q ||| r @> |> L
        Distrib |> L
        distrib_or <@ p @> <@ q @> <@ r @> |> L
]
*)
(*
proof prop_calculus <@ ((p |&| q) ||| (p |&| not q)) = p @> [
    distrib_or_and <@ p |&| q@> <@ p @> <@ not q @> |> LR
]

let ``3.37`` = proof prop_calculus <@ (p |&| q |&| r) =  (p |&| (q |&| r)) @> [
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
    L LeftAssoc
    L LeftAssoc
    commute_and <@ p @> <@ q |&| r @> |> R
    //ident_and_eq_all <@ q @> <@ r @> <@ p @> |> R
    //commute_eq <@q = r@> <@ p @> |> R
    //left_assoc_eq <@ p @> <@ q @> <@ r @> |> R
    //commute_or <@ q ||| r @> <@ p @> |> R
    (*
    left_assoc_or <@ p @> <@ q @> <@ r @> |> R
    right_assoc_eq <@ p = q = r @> <@ q ||| r @> <@ r ||| p @> |> R
    LeftAssoc |> R
    right_assoc_eq <@ p = q = r = (q ||| r) @> <@ r ||| p @> <@ p ||| q@> |> R
    commute_eq <@ ( r ||| p) @> <@ p ||| q @> |> R
    LeftAssoc |> R
    right_assoc_eq <@ p = q = r @> <@ q ||| r @> <@ p ||| q @> |> R
    commute_eq <@ q ||| r @> <@ p ||| q @> |> R
    LeftAssoc |> R
    *)
]
*)
(*
proof prop_calculus <@ ((p ||| q) ||| r ||| s) = ((p ||| r) ||| (q ||| s)) @> [
        left_assoc_or <@ p ||| q@> r' s' |> L
        right_assoc_or p'  q' r' |> L
        commute_or q' r' |> L
        left_assoc_or p' r' q' |> L
    ]
    *)