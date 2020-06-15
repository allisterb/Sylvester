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

let ``9.3a`` = theorem pred_calculus <@ forall x N P = (forall' x (not N ||| P)) @> [
    trade_forall_implies x' N' P'  |> L
    ident_implies_not_or <@ N @> <@ P @> |> L
]

let ``9.3b`` = theorem pred_calculus <@ forall x N P = (forall' x ((N |&| P) = N)) @> [
    trade_forall_implies x' N' P' |> L
    ident_implies_eq_and_eq N' P' |> L
]

let ``9.3c`` = theorem pred_calculus <@ forall x N P = (forall' x ((N ||| P) = P)) @> [
    trade_forall_implies x' <@ N @> <@ P @>  |> L
    def_implies' N' P' |> L
]

let ``9.4a`` = theorem pred_calculus <@ forall x (Q |&| N) P = (forall x Q (N ==> P)) @> [
    trade_forall_implies x' <@ Q |&| N @> P' |> L
    shunt |> QB |> L'
    trade_forall_implies x' Q' <@ N==> P @> |> Commute |> L  
]

let ``9.4b``= theorem pred_calculus <@ forall x (Q |&| N) P = (forall x Q (not N ||| P)) @> [
    trade_forall_implies x' <@ Q |&| N @> P' |> L
    shunt |> QB |> L'
    trade_forall_implies x' <@ Q @> <@ N==> P @> |> Commute |> L
    ident_implies_not_or N' P' |> L
]

let ``9.6`` = theorem pred_calculus <@ forall x N P = (P ||| forall' x (not N))  @> [
    distrib_or_forall |> R
    commute_or P' <@ not N @> |> R
    ident_implies_not_or N' P' |> Commute |> R 
]

let ``9.7`` = theorem pred_calculus <@ not (forall' x (not N)) ==> (forall x N (P |&| Q) = (P |&| forall x N Q)) @> [
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

let ``9.8`` = theorem pred_calculus <@ forall x N true = true @> [
    trade_forall_or_not x' N' <@ true @> |> L
    commute |> L
    zero_or <@ forall' x (not N ) @> |> L 
]

let ``9.9`` = theorem pred_calculus <@ forall x N (P = Q) ==> (forall x N P  = (forall x N Q)) @> [
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

let ``9.10``= theorem pred_calculus <@ (forall x (Q ||| N) P) ==> (forall x Q P) @> [
    split_range_forall |> L
    strengthen_and <@ forall x Q P @> <@ forall x N P @> |> Lemma
]

let ``9.12`` = theorem pred_calculus <@ forall x N (Q ==> P) ==> ((forall x N Q) ==> (forall x N P))@> [
    rshunt |> LR
    collect_forall_and |> L
    commute_and <@ Q ==> P @> Q' |> L
    ident_and_implies Q' P' |> L
    commute_and Q' P' |> L
    strengthen_forall_body_and x' N' P' Q' |> Lemma    
]

let ``9.18a``= theorem pred_calculus <@ not (exists x N (not P)) = forall x N P @> [
  ident_exists_not_forall x' N' <@ not P @> |> L 
  double_negation P' |> L 
  double_negation <@ forall x N P @> |> L
]

let ``9.18b``= theorem pred_calculus <@ not (exists x N P) = forall x N (not P) @> [
  ident_exists_not_forall x' N' P' |> L 
  double_negation <@ forall x N (not P) @> |> L 
]

let ``9.18c`` = theorem pred_calculus <@ exists x N (not P) = not (forall x N P) @> [
  ident_not_exists_forall x' N' P' |> Commute |> R
  double_negation <@ exists x N (not P) @> |> R
]

let ``9.19``= theorem pred_calculus <@ exists x N P = (exists' x (N |&| P)) @> [
    Dual |> L
    Dual |> R
    trade_body |> LR |> LR' |> L'
    distrib_not_and N' P' |> R
    ident_implies_not_or N' <@ not P@> |> LR |> LR' |> L'
]

let ``9.20``= theorem pred_calculus <@ exists x (Q |&| N) P = (exists x Q (N |&| P)) @> [
   trade_exists_and x' <@ Q |&| N @> P' |> L
   right_assoc_and Q' N' P' |> LR |> L'
   trade_exists_and x' Q' <@ N |&| P @> |> Commute |> L
]

let ``9.21`` = theorem pred_calculus <@ P |&| exists x N Q = (exists x N (P |&| Q))@> [
    Dual |> R |> L'
    double_negation P' |> Commute |> L |> L'
    collect_not_or <@ not P @> <@ forall x N (not Q) @> |> L
    distrib_or_forall' x' N' <@ not P @> <@ not Q @> |> LR |> LR' |> L'
    Dual |> R
    distrib_not_and P' Q' |> LR |> LR' |> R' 
]

let ``9.22`` = theorem pred_calculus <@ exists x N P = (P |&| (exists' x N)) @> [
    do failIfOccursFree x' P'
    distrib_and_exists_and x' <@ true @> P' N' |> R
    trade_exists_and x' N' P' |> L
    commute_and N' P' |> LR |> L'
]

let ``9.23`` = proof pred_calculus <@ exists' x N ==> ((exists x N (P ||| Q)) = (P ||| exists x N Q)) @> [
    distrib_and_exists x' N' <@ P ||| Q @> |> L |> R'
    distrib_and_or <@ exists' x N @> P' Q' |> CommuteL |> L |> R'
    distrib_and_exists x' N' Q' |> Commute |> CommuteL |> L |> R'
    axiom prop_calculus <@ exists' x N ==> exists' x N @> |> Deduce
    ident_and <@ P @> |> CommuteL |> LR
    def_true <@ P ||| (exists x N Q) @> |> Commute |> R
]

let ``9.24`` = proof pred_calculus <@ exists x N (false) = false @> [
    distrib_and_exists x' N' <@ false @> |> L
    commute |> L
    zero_and <@ exists' x N @> |> L
]