#load "Include.fsx"

open Sylvester
open PropCalculus

/// true = (p = p)
let p,q,r,s = var4<bool>
let p',q',r',s' = <@ p @>, <@ q @>, <@ r @>, <@ s @>


proof prop_calculus <@ (p |&| q |&| r) = (p |&| (q |&| r)) @> [     
    golden_rule p' q' |> L
    golden_rule <@ (p = q) = (p ||| q) @> r' |> L 
    commute_or <@ ((p = q) = (p ||| q)) @> r' |> L
    distrib_or_eq r' <@ p = q @> <@ p ||| q @> |> L
    distrib_or_eq r' p' q' |> L
    right_assoc_eq <@ p = q@> <@ p ||| q@> r' |> L
    commute_eq <@ p ||| q @> r' |> L
    commute_or r' q' |> L
    commute_eq <@ r ||| p @> <@ q ||| r @> |> L
    commute_or r' <@ p ||| q @> |> L
    left_assoc_eq <@ p = q @> r' <@ p ||| q @> |> L
    LeftAssoc |> L
    left_assoc_eq <@ p = q = r = (p ||| q) @> <@ q ||| r @> <@r ||| p@> |> L
    //LeftAssoc |> L
    //left_assoc_eq <@ (p ||| q) @> <@ q ||| r @> <@(r ||| p)@> |> L
    commute_and p' <@ q |&| r @> |> R
    ident_and_eq_all q' r' p' |> R
    commute_eq <@ q = r @> p' |> R
    left_assoc_eq <@ p = q = r = (p ||| q) @> <@ q ||| r @> <@ r ||| p @> |> L
    left_assoc_eq p' q' r' |> R
    commute_or <@ q ||| r @> p' |> R
    left_assoc_or p' q' r' |> R
    right_assoc_eq <@ p = q = r @> <@ q ||| r @> <@ r ||| p @> |> R
    left_assoc_eq <@ p = q = r @>  <@ q ||| r @> <@ r ||| p @> |> R
    right_assoc_eq <@ p = q = r = (q ||| r) @> <@ r ||| p @> <@ p ||| q @> |> R
    commute_eq <@ (r ||| p) @> <@ p ||| q @> |> R
    LeftAssoc |> R
    left_assoc_eq <@ p = q = r = (q ||| r) @> <@ p ||| q  @> <@ r ||| p @> |> R
    right_assoc_eq <@ p = q = r @>  <@ q ||| r @> <@ p ||| q @> |> R
    commute_eq <@ q ||| r @> <@ p ||| q @> |> R
    left_assoc_eq <@ p = q = r @> <@ p ||| q @>  <@ (q ||| r) @> |> R

] |> last_state |> expand_left |> expand_left  |> expand_left |> expand_left |> src