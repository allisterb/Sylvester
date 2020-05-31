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

(*
proof prop_calculus <@ p |&| q ==> (p = q) @> [
    deduce <@ p |&| q ==> p @> [
        strenghten_and p' q' |> Lemma
    ]
    deduce <@ p |&| q ==> q @> [
        commute |> L
        strenghten_and q' p' |> Lemma
    ]
    def_true <@ true @> |> Commute |> R
]
*)

//let lemma1 = theorem prop_calculus <@ (p ==> r) ==> (p = (p |&| r)) @> [
//  commute |> R
//  ident_implies_eq_and_eq p' r' |> L
//  reflex_implies <@ p |&| r = p @> |> Lemma
//]

proof prop_calculus <@ (p ==> r) ==> ((q ==> s) ==> (p |&| q ==> r |&| s)) @> [
  //shunt' <@ p ==> r @> <@ q ==> s @> <@ (p |&| q ==> r |&| s) @> |> Commute |> LR 
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
  //shunt' <@ p ==> r @> <@ q ==> s @> <@ (p |&| q ==> r |&| s) @> |> LR
  //weaken_and_or <@ p ==> r @> <@ q ==> s @> |> Lemma
] 


//pff.Conjuncts