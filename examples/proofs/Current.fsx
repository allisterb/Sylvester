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

proof pred_calculus <@ forall x N (Q ==> P) ==> ((forall x N Q) ==> (forall x N P))@> [
    //def_implies' Q' P' |> L
    //ident_and_implies Q' P' |> L
    //def_implies' Q' P' |> L
    //shunt' <@ forall x N (Q ==> P) @> <@ forall x N Q @> <@ forall x N P @> |> Commute |> LR
    //def_implies' Q' P' |> L
    //ident_forall_eq x' N' <@ Q ||| P @> P' |> L
    //distrib_forall_and' x' N' P' Q' |> L
    //strenghten_and <@ forall x N P @> <@ forall x N Q @> |> Lemma
]

