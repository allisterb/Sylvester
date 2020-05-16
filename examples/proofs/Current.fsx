#load "Include.fsx"

open Sylvester
open PropCalculus

let p,q,r,s = var4<bool> 
let p',q',r',s' = <@ p @>, <@ q @>, <@ r @>, <@ s @>

proof prop_calculus <@ (p ==> q) |&| (q ==> p) = (p = q) @> [
    ident_implies_not_or p' q' |> L |> L'
    ident_implies_not_or q' p' |> R |> L'
    distrib |> L
    commute |> L |> L'
    commute |> R |> L'
    distrib |> L |> L'
    distrib |> R |> L'
    contr q' |> CommuteL |> L
    contr p' |> L
    ident_or <@ p |&| q@> |> CommuteL |> L
    ident_or <@ not q |&| not p @> |> L
    //golden_rule |> L
    //left_assoc |> R |> L'
    //excluded_middle' q' |> L 

] |> left_state |> expand_left |> src