#load "Include.fsx"

open Sylvester
open PropCalculus

let p,q,r,s = var4<bool> 
let p',q',r',s' = <@ p @>, <@ q @>, <@ r @>, <@ s @>

proof prop_calculus <@ (p ||| q ==> p |&| q) = p = q@> [
    right_assoc |> LR 
    right_assoc |> LR
    //right_assoc |> LR 
    //left_assoc |> LR
    //golden_rule' |> R |> L'
] |> last_state |> expand_left |> src
(*
proof prop_calculus <@ (p |&| (q ==> p)) = p @> [
    def_implies' |> R |> L'
    distrib_and_eq p' <@q ||| p @> p' |> L
    distrib |> L |> L' |> L'  
    //distrib_and_or p' q' p' |> CommuteL |> L
] |> last_state |> expand_left |> expand_left |> src

proof prop_calculus <@ p ||| (p ==> q) = true @> [
    ident_implies_not_or p' q' |> L
    left_assoc |> L
    //distrib_or_or p' <@ not p @> q' |> L
    //ident_implies_eq_and_eq p' q' |> L
    //distrib_and_eq p' <@ p |&| q @> p' |> L
    //left_assoc |> L |> L' |> L'
    //idemp_and p' |> L 
]
*)
//weakening p' q'