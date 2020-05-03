#load "Include.fsx"

open Sylvester
open PropCalculus

/// true = (p = p)
let p,q,r,s = var4<bool>

(*
proof prop_calculus <@(p |&| (not p ||| q)) = (p |&| q) @> [
    GoldenRule |> L
    left_assoc_or <@ p @> <@ not p @> <@ q @> |> L
    ExcludedMiddle |> L
    //GoldenRule |> R
    //commute_or <@ p @> <@ not p ||| q @> |> L
] |> last_state |> expand_left |> expand_left |> src
*)

proof prop_calculus <@ ((p ||| q) ||| (r ||| s)) = ((p ||| r) ||| (q ||| s)) @> [
    left_assoc_or <@ p ||| q@> <@ r @> <@ s @> |> L
    //LeftAssoc |> L
    //commute_or <@ q @> <@ r @> |> L
] |> last_state |> expand_left |> expand_left



