#load "Include.fsx"

open Sylvester
open PropCalculus

/// true = (p = p)
let p,q,r,s = var4<bool>
let p',q',r',s' = <@ p @>, <@ q @>, <@ r @>, <@ s @>

let ``3.59`` = proof prop_calculus <@ p ==> q = (not p ||| q) @> [
    implication |> L
    ident_or_not_or q' p' |> CommuteL |> R
    commute |> R
    commute |> L |> R'
]