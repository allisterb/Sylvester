#load "Include.fsx"

open Sylvester
open PropCalculus
open PredCalculus

let p,q,r,s = var4<bool>
let x,y = var2<bool> 
let p',q',r',s' = <@ p @>, <@ q @>, <@ r @>, <@ s @>
let x' = <@ x @>
let y' = <@ y @>

let P,Q, N, A= var4<bool>
let P',Q', N' = <@ P @>, <@ Q @>, <@ N @>

Proof.LogLevel <- 0
let pzz = proof prop_calculus <@ (p ==> r) ==> ((q ==> s) ==> (p |&| q ==> (r |&| s))) @> [
    shunt' <@ p ==> r @> <@ q ==> s @> <@ (p |&| q ==> (r |&| s)) @> |> Commute |> LR 
    deduce' <@ (p ==> r) ==> (p = (p |&| r)) @> [
        commute |> R
        ident_implies_eq_and_eq p' r' |> L
        reflex_implies <@ p |&| r = p @> |> Lemma
    ]
    deduce' <@ (q ==> s) ==> (q = (q |&| s)) @> [
        commute |> R
        ident_implies_eq_and_eq q' s' |> L
        reflex_implies <@ q |&| s = q @> |> Lemma
    ]
    
    right_assoc_and p' r' <@ q |&| s @> |> R
    left_assoc_and r' q' s'|>  R
    commute_and r' q' |> R
    right_assoc_and q' r' s' |> R
    left_assoc_and p' q' <@ r |&| s @> |> R
    
    commute |> L |> R'
    strenghten_and <@ r |&| s @> <@p |&| q @> |> Lemma
]


pzz |> right_state |> src