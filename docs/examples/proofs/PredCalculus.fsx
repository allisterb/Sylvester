#load "Include.fsx"

open Sylvester
open PropCalculus
open PredCalculus

let p,q,r,s = var4'<bool> "p" "q" "r" "s"
let x,y = var2'<bool> "x" "y"
let P,N,Q,S = var4'<bool> "P" "N" "Q" "S"
let d = boolvar "d"
let H, F = pred<bool>, pred<bool>

let ``9.xx`` = theorem pred_calculus <@ (forall' %x (H %x ==> not (F %x)) |&| H %d) ==> not (F %d)@> [
    apply shunt 
]


let ``9.3a`` = theorem pred_calculus <@ forall %x %N %P = (forall' %x (not %N ||| %P)) @> [
    trade_forall_implies x N P  |> apply_left
    ident_implies_not_or N P |> apply_left
]

let ``9.3b`` = theorem pred_calculus <@ forall %x %N %P = (forall' %x ((%N |&| %P) = %N)) @> [
    trade_forall_implies x N P |> apply_left
    ident_implies_eq_and_eq N P |> apply_left
]

let ``9.3c`` = theorem pred_calculus <@ forall %x %N %P = (forall' %x ((%N ||| %P) = %P)) @> [
    trade_forall_implies x N P |> apply_left
    def_implies' N P |> apply_left
]

let ``9.4a`` = theorem pred_calculus <@ forall %x (%Q |&| %N) %P = (forall %x %Q (%N ==> %P)) @> [
    trade_forall_implies x <@ %Q |&| %N @> P |> apply_left
    shunt |> QB |> after_left
    trade_forall_implies x Q <@ %N==> %P @> |> Commute |> apply_left  
]


let ``9.4b``= theorem pred_calculus <@ forall %x (%Q |&| %N) %P = (forall %x %Q (not %N ||| %P)) @> [
    trade_forall_implies x <@ %Q |&| %N @> P |> apply_left
    shunt |> QB |> after_left
    trade_forall_implies x <@ %Q @> <@ %N==> %P @> |> Commute |> apply_left
    ident_implies_not_or N P |> apply_left
]



let ``9.6`` = theorem pred_calculus <@ forall %x %N %P = (%P ||| forall' %x (not %N))  @> [
    distrib_or_forall |> apply_right
    commute_or P <@ not %N @> |> apply_right
    ident_implies_not_or N P |> Commute |> apply_right 
]

let ``9.7`` = theorem pred_calculus <@ not (forall' %x (not %N)) ==> (forall %x %N (%P |&| %Q) = (%P |&| forall %x %N %Q)) @> [
    let lemma1 = lemma pred_calculus <@ not (forall' %x (not %N)) ==> (forall' %x (not %N) = false) @> [
        distrib_implies_eq_and <@ (not (forall' %x (not %N))) @> <@ forall' %x (not %N) @> <@ false @> |> apply
        contr <@ forall' %x (not %N) @> |> CommuteL |> apply_left
        zero_and <@ not (forall' %x (not %N)) @> |> apply_right   
    ]
    distrib_forall_and' x N P Q |> apply_right
    trade_forall_or_not x N P |> apply_right
    deduce' lemma1 |> apply_right
    ident_or P |> apply_right
    def_true <@ %P |&| forall %x %N %Q @> |> Commute |> apply_right
] 


let ``9.8`` = theorem pred_calculus <@ forall %x %N true = true @> [
    trade_forall_implies x N <@ true @> |> apply_left
    implies_true  N |> Taut |> apply_left
    trade_forall_or_not x <@ true @> <@ true @> |> apply_left
    commute |> apply_left
    zero_or <@ forall' %x (not true ) @> |> apply_left
]

let ``9.9`` = theorem pred_calculus <@ forall %x %N (%P = %Q) ==> (forall %x %N %P  = (forall %x %N %Q)) @> [
     distrib_implies_eq_and <@ forall %x %N (%P = %Q) @> <@ forall %x %N %P @> <@ forall %x %N %Q @> |> apply
     collect_forall_and |> apply_left
     collect_forall_and |> apply_right
     commute_and <@ %P = %Q @> P |> apply_left
     commute_and <@ %P = %Q @> Q |> apply_right
     commute_eq P Q |> apply_left
     ident_and_eq P Q |> apply_left
     ident_and_eq Q P |> apply_right
     commute_and Q P |> apply_right
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

let ``9.16a``= proof pred_calculus <@ P ==> forall' x P @> [
    axiom pred_calculus <@ P ==> P @> |> Deduce |> R
    ident_forall_true' x' |> R
]

let ``9.16b`` = theorem pred_calculus <@ forall' x P ==> P @> [
    inst' x' P' |> L
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
    dual |> L
    dual |> R
    trade_body |> LR |> LR' |> L'
    distrib_not_and N' P' |> R
    ident_implies_not_or N' <@ not P@> |> L
]

let ``9.20``= theorem pred_calculus <@ exists x (Q |&| N) P = (exists x Q (N |&| P)) @> [
   trade_exists_and x' <@ Q |&| N @> P' |> L
   right_assoc_and Q' N' P' |> LR |> L'
   trade_exists_and x' Q' <@ N |&| P @> |> Commute |> L
]

let ``9.21`` = theorem pred_calculus <@ P |&| exists x N Q = (exists x N (P |&| Q))@> [
    dual |> R |> L'
    double_negation P' |> Commute |> L |> L'
    collect_not_or <@ not P @> <@ forall x N (not Q) @> |> L
    distrib_or_forall' x' N' <@ not P @> <@ not Q @> |> LR |> LR' |> L'
    dual |> R
    distrib_not_and P' Q' |> LR |> LR' |> R' 
]

let ``9.22`` = theorem pred_calculus <@ exists x N P = (P |&| (exists' x N)) @> [
    do fail_if_occurs_free x' P'
    distrib_and_exists_and x' <@ true @> P' N' |> R
    trade_exists_and x' N' P' |> L
    commute_and N' P' |> LR |> L'
]

let ``9.23`` = theorem pred_calculus <@ exists' x N ==> ((exists x N (P ||| Q)) = (P ||| exists x N Q)) @> [
    distrib_and_exists x' N' <@ P ||| Q @> |> L |> R'
    distrib_and_or <@ exists' x N @> P' Q' |> CommuteL |> L |> R'
    distrib_and_exists x' N' Q' |> Commute |> CommuteL |> L |> R'
    axiom prop_calculus <@ exists' x N ==> exists' x N @> |> Deduce |> L |> R'
    ident_and <@ P @> |> CommuteL |> L |> R'
    def_true <@ P ||| (exists x N Q) @> |> Commute |> R
]

let ``9.24`` = theorem pred_calculus <@ exists x N (false) = false @> [
    distrib_and_exists x' N' <@ false @> |> L
    commute |> L
    zero_and <@ exists' x N @> |> L
]

let ``9.25`` = theorem pred_calculus <@ exists x N P ==> (exists x (Q ||| N) P) @> [
    split_range_exists |> R
    commute |> R
    weaken_or <@ exists x N P @> <@ exists x Q P @> |> Lemma   
]

let ``9.26`` = theorem pred_calculus <@ exists x N P ==> (exists x N (P ||| Q)) @> [
    distrib_exists_or' x' N' P' Q' |> R 
    weaken_or <@ exists x N P @> <@ exists x N Q @> |> Lemma
]

let ``9.29`` = theorem pred_calculus <@ exists' x (forall' y P) ==> (forall' y (exists' x P)) @> [
    def_implies |> LR
    distrib_or_forall |> L
    collect_exists_or' x' <@ true @> <@ forall y true P @> P' |> QB |> L'
    inst' y' P' |> L |> QB' |> QB' |> L'
    idemp |> QB |> QB' |> L'
    let lemma1 = ident pred_calculus <@ forall' y P ||| P = P @> [
        def_implies'<@ (forall' y P) @> P' |> Commute |> LR
        forall_implies_inst' y' P' |> Lemma
    ]
    lemma1 |> QB |> QB' |> L' 
]

(*
let lemma1 = proof pred_calculus <@ Q ==> exists x' N (Q ==> P) = (Q ==> exists x' N P)@> [
    axiom pred_calculus <@Q ==> P = {@> |> Deduce
]
proof pred_calculus <@ exists x N (forall y Q P) ==> (forall y Q (exists x N P)) @> [ 
    def_implies |> LR
    distrib_or_forall |> L
    collect_exists_or' x' N' <@ forall y Q P @> P' |> QB |> L'
    ident_forall_inst y' Q' P' |> L
    ident_or_conseq P' Q'  |> CommuteL |> L
    ident_forall_inst y' Q' <@exists x N (Q ==> P)@> |> L
    ident_forall_inst y' Q' <@exists x N P@> |> R
]       
*)