#load "Include.fsx"

open Sylvester
open PropCalculus
open PredCalculus

/// true = (p = p)
let p,q,r,s = var4<bool>
let x,y = var2<bool>
let x' = <@ x @>
let y' = <@ y @>
let p',q',r',s' = <@ p @>, <@ q @>, <@ r @>, <@ s @>
let P,N,Q,S = var4<bool>
let P', Q', N' = <@ P @>, <@ Q @>, <@ N @>

let ``9.3a`` = proof pred_calculus <@ forall x N P = (forall' x (not N ||| P)) @> [
    trading_forall x' N' P'  |> L
    PropCalculus.ident_implies_not_or <@ N @> <@ P @> |> L
]

let ``9.3b`` = proof pred_calculus <@ forall x N P = (forall' x ((N |&| P) = N)) @> [
    trading_forall x' N' P' |> L
    PropCalculus.ident_implies_eq_and_eq N' P' |> L
]

let ``9.3c`` = proof pred_calculus <@ forall x N P = (forall' x ((N ||| P) = P)) @> [
    trading_forall x' <@ N @> <@ P @>  |> L
    PropCalculus.def_implies' N' P' |> L
]

let ``9.4a`` = proof pred_calculus <@ forall x (Q |&| N) P = (forall x Q (N ==> P)) @> [
    trading_forall x' <@ Q |&| N @> P' |> L
    shunt' Q' N' P' |> L
    trading_forall x' Q' <@ N==> P @> |> Commute |> L  
]

let ``9.4b``= proof pred_calculus <@ forall x (Q |&| N) P = (forall x Q (not N ||| P)) @> [
    trading_forall x' <@ Q |&| N @> P' |> L
    shunt' Q' N' P' |> L
    trading_forall x' <@ Q @> <@ N==> P @> |> Commute |> L
    ident_implies_not_or N' P' |> L
]

let ``9.6`` = proof pred_calculus <@ forall x N P = (P ||| forall' x (not N))  @> [
    distrib_forall |> R
    commute_or P' <@ not N @> |> R
    ident_implies_not_or N' P' |> Commute |> R 
]