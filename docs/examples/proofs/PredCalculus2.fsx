#load "Include.fsx"

open Sylvester
open PropCalculus
open PredCalculus

let p,q,r,s = var4'<bool> "p" "q" "r" "s"
let x,y = var2'<bool> "x" "y"
//let P,N,Q,S = var4'<bool> "P" "N" "Q" "S"

let d = boolvar "d"
let H, F = pred<bool>, pred<bool>
let P, Q, N = pred<bool>, pred<bool>, pred<bool>

/// forall x N P = (forall x true (N ==> P))
let trade_forall_implies x N P = id_ax pred_calculus <@ forall %x (N %x) (P %x) = (forall %x true (N %x ==> P %x)) @>

/// forall x (Q |&| N) P = (forall x Q (N ==> P))
let trade_forall_and_implies x Q N P = ident pred_calculus <@ forall %x ((Q %x) |&| (N %x)) (P %x) = (forall %x (Q %x) (N %x ==> P %x)) @> [
    //trade_forall_implies x <@ Q %x |&| N %x @> P |> L
    //shunt |> QB |> L'
    //trade_forall_implies x Q  <@ N %x ==> P %x @> |> Commute |> L
]
