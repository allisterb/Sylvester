#load "Include.fsx"

open Sylvester
open PropCalculus

let p,q,r,s = var4<bool>
let p',q',r',s' = <@ p @>, <@ q @>, <@ r @>, <@ s @>
let P,N,A,S = var4<bool>
 

let ``3.52`` = theorem prop_calculus <@ (p = q) = (p |&| q) ||| (not p |&| not q) @> [
        collect |> R
        commute |> L |> L'
        commute |> L
        commute |> R |> L'
        golden_rule' p' q' |> LeftAssoc |> L
]

let ``3.59`` = theorem prop_calculus <@ p ==> q = (not p ||| q) @> [
    def_implies |> L
    ident_or_not_or q' p' |> CommuteL |> R
    commute |> R
    commute |> L |> R'
]

let ``3.60`` = theorem prop_calculus <@ p ==> q = ((p |&| q) = p) @> [
    def_implies |> L
    commute |> LR
    right_assoc |> LR
    commute |> R |> R' 
    left_assoc |> R 
]

let ``3.61`` = theorem prop_calculus <@ p ==> q = (not q ==> not p) @> [
    def_implies |> R
    commute |> R
    commute |> R |> R'
    distrib_not_and p' q' |> Commute |> R |> R'
    symm_eq_not_eq p' <@ p |&| q @> |> Commute |> R 
    commute |> R
    ident_implies_eq_and_eq p' q' |> Lemma'
]

let ``3.62`` = theorem prop_calculus <@p ==> (q = r) = ((p |&| q) = (p |&| r))@> [
    ident_implies_eq_and_eq p' <@ q = r @> |> L
    distrib_and_eq p' q' r' |> L
]

let ``3.71`` = theorem prop_calculus <@ p ==> p @> [
    def_implies |> LR
]


let ``3.76a`` = theorem prop_calculus <@ p ==> (p ||| q) @> [
    def_implies |> LR
    left_assoc |> L
    idemp_or p' |> L
]

let ``3.76c`` = theorem prop_calculus <@ p |&| q ==> p ||| q @> [
    def_implies |> L
    commute |> L |> L'
    distrib |> L |> L'
    commute |> LR
    idemp_or p' |> LR
    distrib |> L |> R'
    idemp_and p' |> LR
    distrib |> LR
    distrib |> R |> L'
    distrib |> L
    idemp_or p' |> L
    distrib |> L
    commute_or q' p' |> L
    idemp_and <@ p ||| q @> |> L
    commute |> L |> L'
    distrib |> L |> L'
    idemp_and q' |> L
    absorb_or q' p' |> CommuteL |> L
    commute |> R |> L'
    left_assoc |> L
    idemp_or q' |> L
] 

theorem prop_calculus <@  (p |&| q ) ==> p @> [
    def_implies |> LR
    commute |> L
    absorb_or p' q' |> Lemma'
]

theorem prop_calculus <@ p ==> true @> [
    def_implies |> LR
    zero_or p' |> L
    //ident_eq <@ true @> |> L
]

let ``3.76d`` = theorem prop_calculus <@ (p ||| (q |&| r)) ==> (p ||| q) @> [
    distrib |> L
    strengthen_and <@ p ||| q @> <@ p ||| r @> |> Lemma
]

let ``3.76e`` = theorem prop_calculus <@ (p |&| q)  ==> (p |&| (q ||| r)) @> [
    distrib |> R
    weaken_or <@ p |&| q @> <@ p |&| r @> |> Lemma
]

(*
let ``3.77`` = theorem prop_calculus <@ p |&| (p ==> q) ==> q @> [
    ident_and_implies p' q' |> L
    commute_and p' q' |> LR
    strengthen_and q' p' |> Taut |> LR
]
*)
let ``3.78`` = theorem prop_calculus <@( p ==> r) |&| (q ==> r) = (p ||| q  ==> r) @> [
    ident_implies_not_or <@p ||| q@> r' |> R
    distrib|> L |> R'
    distrib_or_and r' <@ not p @> <@ not q@> |> CommuteL |> R
    commute |> L |> R'
    commute |> R |> R'
    ident_implies_not_or p' r' |> Commute |> R
    ident_implies_not_or q' r' |> Commute |> R
]

let ``3.79`` = theorem prop_calculus <@ (p ==> r) |&| (not p ==> r ) = r @> [
    case_analysis_1 p' <@not p @> r' |> LR
    excluded_middle |> L |> L'
    ident_conseq_true r' |> Lemma'
]

let ``3.81`` = theorem prop_calculus <@ (p ==> q) |&| (q ==> p) ==> (p = q) @> [
  mutual_implication' p' q' |> L  
  reflex_implies <@ p = q @> |> Lemma
] 

let ``3.82b`` = theorem prop_calculus <@ (p = q) |&| (q ==> r) ==> (p ==> r) @> [
    mutual_implication' p' q' |> Commute |> L
    shunt' <@ (p ==> q) |&| (q ==> p) |&| (q ==> r)@> p' r' |> Commute |> LR
    commute |> L
    left_assoc |> L
    left_assoc |> L |> L'
    ident_and_implies p' q' |> L |> L'
    right_assoc |> L |> L'
    ident_and_implies q' p' |> L |> L'
    commute |> R |> L' |> L'
    left_assoc |> L |> L'
    idemp_and p' |> L |> L'
    right_assoc |> L
    ident_and_implies q' r' |> L
    left_assoc |> L
    commute |> L
    strengthen_and r' <@ p |&| q @> |> Lemma
]
let ``3.84`` = theorem prop_calculus <@ p |&| q ==> r = (p ==> (q ==> r)) @> [
    ident_implies_eq_and_eq <@ p |&| q@> r' |> L
    ident_implies_eq_and_eq q' r' |> R
    ident_implies_eq_and_eq p' <@q |&| r = q@> |> R
    distrib_and_eq p' <@q |&| r@> <@ q @> |> R
    left_assoc_and p' q' r' |> R
    right_assoc |> R
    def_true p' |> Commute |> R
    left_assoc |> LR
    commute |> LR
]

let ``3.84a`` = theorem prop_calculus <@ p |&| q ==> (p |&| (q ||| r)) @> [
    subst_true |> LR
    commute |> L
    subst_true |> LR
    distrib |> R
    idemp_and <@ true @> |> L |> R'
    zero_or <@ true |&| r@> |> CommuteL |> R
    implies_true <@ q |&| p @> |> Lemma
]

let ``3.84b`` = theorem prop_calculus <@ p ==> (q ==> p) @> [
    def_implies |> R
    def_implies |> LR
    commute |> L |> R' |> L' 
    distrib |> L
    left_assoc |> L |> L'
    idemp_or p' |> L |> L' |> L'
    commute |> L |> L' 
    idemp_or p' |> R |> L'
] 
let ``4.1`` = theorem prop_calculus <@ (p ==> q) ==> ((p ||| r) ==> (q ||| r)) @> [
    def_implies |> R
    commute_or_or p' r' q' r' |> L |> R'
    idemp_or r' |> L |> R'
    commute_or <@ p ||| q @> r' |> L |> R'
    commute_or q' r' |> R |> R'
    collect_or_eq r' <@ p ||| q @> q' |> R
    commute |> R
    def_implies' p' q' |> Commute |> R
    weaken_or <@ p ==> q @> r' |> Lemma
]

let ``4.4a`` = theorem prop_calculus <@ p |&| q ==> (p = q) @> [
    let lemma1 = lemma prop_calculus <@ p |&| q ==> p @> [
        strengthen_and p' q' |> Lemma
    ]
    let lemma2 = lemma prop_calculus <@ p |&| q ==> q @> [
        commute |> L
        strengthen_and q' p' |> Lemma
    ]
    Deduce lemma1
    Deduce lemma2
    def_true <@ true @> |> Commute |> R
    implies_true <@ p |&| q @> |> Lemma
]

let ``4.4b`` = theorem prop_calculus <@ (p ==> r) ==> ((q ==> s) ==> (p |&| q ==> (r |&| s))) @> [
    shunt' <@ p ==> r @> <@ q ==> s @> <@ (p |&| q ==> (r |&| s)) @> |> Commute |> LR 
    let lemma1 = lemma prop_calculus <@ (p ==> r) ==> (p = (p |&| r)) @> [
        commute |> R
        ident_implies_eq_and_eq p' r' |> L
        reflex_implies <@ p |&| r = p @> |> Lemma
    ]
    let lemma2 = lemma prop_calculus <@ (q ==> s) ==> (q = (q |&| s)) @> [
        commute |> R
        ident_implies_eq_and_eq q' s' |> L
        reflex_implies <@ q |&| s = q @> |> Lemma
    ]
    Deduce' lemma1
    Deduce' lemma2
    right_assoc_and p' r' <@ q |&| s @> |> R
    left_assoc_and r' q' s'|>  R
    commute_and r' q' |> R
    right_assoc_and q' r' s' |> R
    left_assoc_and p' q' <@ r |&| s @> |> R
    
    commute |> L |> R'
    strengthen_and <@ r |&| s @> <@p |&| q @> |> Taut |> R
]