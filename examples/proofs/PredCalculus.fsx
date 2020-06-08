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
    trade_forall_implies x' N' P'  |> L
    ident_implies_not_or <@ N @> <@ P @> |> L
]

let ``9.3b`` = proof pred_calculus <@ forall x N P = (forall' x ((N |&| P) = N)) @> [
    trade_forall_implies x' N' P' |> L
    ident_implies_eq_and_eq N' P' |> L
]

let ``9.3c`` = proof pred_calculus <@ forall x N P = (forall' x ((N ||| P) = P)) @> [
    trade_forall_implies x' <@ N @> <@ P @>  |> L
    PropCalculus.def_implies' N' P' |> L
]

let ``9.4a`` = proof pred_calculus <@ forall x (Q |&| N) P = (forall x Q (N ==> P)) @> [
    trade_forall_implies x' <@ Q |&| N @> P' |> L
    shunt' Q' N' P' |> L
    trade_forall_implies x' Q' <@ N==> P @> |> Commute |> L  
]

let ``9.4b``= proof pred_calculus <@ forall x (Q |&| N) P = (forall x Q (not N ||| P)) @> [
    trade_forall_implies x' <@ Q |&| N @> P' |> L
    shunt' Q' N' P' |> L
    trade_forall_implies x' <@ Q @> <@ N==> P @> |> Commute |> L
    ident_implies_not_or N' P' |> L
]

let ``9.6`` = proof pred_calculus <@ forall x N P = (P ||| forall' x (not N))  @> [
    distrib_or_forall |> R
    commute_or P' <@ not N @> |> R
    ident_implies_not_or N' P' |> Commute |> R 
]

let ``9.7`` = proof pred_calculus <@ not (forall' x (not N)) ==> (forall x N (P |&| Q) = (P |&| forall x N Q)) @> [
    let lemma1 = proof pred_calculus <@ not (forall' x (not N)) ==> (forall' x (not N) = false) @> [
        distrib_implies_eq_and <@ (not (forall' x (not N))) @> <@ forall' x (not N) @> <@ false @> |> LR
        contr <@ forall' x (not N) @> |> CommuteL |> L
        zero_and <@ not (forall' x (not N)) @> |> R   
    ]
    distrib_forall_and' x' N' P' Q' |> R
    trade_forall_or_not x' N' P' |> R
    deduce' lemma1
    ident_or P' |> R
    def_true <@ P |&| forall x N Q @> |> Commute |> R
] 

let ``9.8`` = proof pred_calculus <@ (forall x N true) = true @> [
    trade_forall_or_not x' N' <@ true @> |> L
    commute |> L
    zero_or <@ forall' x (not N ) @> |> L 
]

let ``9.9`` = proof pred_calculus <@ forall x N (P = Q) ==> (forall x N P  = (forall x N Q)) @> [
     distrib_implies_eq_and <@ forall x N (P = Q) @> <@ forall x N P @> <@ forall x N Q @> |> LR
     collect_forall_and |> L
     collect_forall_and |> R
     commute_and <@ P = Q @> P' |> L
     commute_and <@ P = Q @> Q' |> R
     commute_eq P' Q' |> L
     ident_and_eq P' Q' |> L
     ident_and_eq Q' P' |> R
     commute_and Q' P' |> R
]

let ``9.10``= proof pred_calculus <@ (forall x (Q ||| N) P) ==> (forall x Q P) @> [
    split_range_forall |> L
    strenghten_and <@ forall x Q P @> <@ forall x N P @> |> Lemma
]