#load "Include.fsx"

open Sylvester
open PropCalculus

let p,q,r,s = boolvar4 "p" "q" "r" "s"
let P,N,A,S = boolvar4 "P" "N" "A" "S"

let ``3.76a`` = theorem prop_calculus (p ==> (p + q))  [
    def_implies |> apply
    left_assoc |> apply_left
    idemp_or p |> apply_left
]

(*
let ``3.52`` = theorem prop_calculus ((p == q) == (p * q) + (!!p * !!q)) [
        collect |> apply_right
        commute |> apply_left |> after_left
        commute |> apply_left
        commute |> apply_right |> after_left
        golden_rule' p q |> LeftAssoc |> apply_left
]
*)

let ``3.59`` = theorem prop_calculus (p ==> q == (!!p + q) ) [
    def_implies |> apply_left
    ident_or_not_or q p |> CommuteL |> apply_right
    commute |> apply_right
    commute |> apply_left |> after_right
]

let ``3.60`` = theorem prop_calculus (p ==> q == ((p * q) == p)) [
    def_implies |> apply_left
    commute |> apply
    right_assoc |> apply
    commute |> apply_right |> after_right 
    left_assoc |> apply_right 
]

(*
let ``3.76c`` = theorem prop_calculus <@ %p |&| %q ==> %p ||| %q @> [
    def_implies |> apply_left
    commute |> apply_left |> after_left
    distrib |> apply_left |> after_left
    commute |> apply
    idemp_or p |> apply
    distrib |> apply_left |> after_right
    idemp_and p |> apply
    distrib |> apply
    distrib |> apply_right |> after_left
    distrib |> apply_left
    idemp_or p |> apply_left
    distrib |> apply_left
    commute_or q p |> apply_left
    idemp_and <@ %p ||| %q @> |> apply_left
    commute |> apply_left |> after_left
    distrib |> apply_left |> after_left
    idemp_and q |> apply_left
    absorb_or q p |> CommuteL |> apply_left
    commute |> apply_right |> after_left
    left_assoc |> apply_left
    idemp_or q |> apply_left
] 

let ``3.61`` = theorem prop_calculus (p ==> q == (!! q ==> !! p)) [
    def_implies |> apply_right
    commute |> apply_right
    commute |> apply_right |> after_right
    distrib_not_and p q |> Commute |> apply_right |> after_right
    symm_eq_not_eq p <@ %p |&| %q @> |> Commute |> apply_right 
    commute |> apply_right
    ident_implies_eq_and_eq p q |> Lemma'
]

let ``3.62`` = theorem prop_calculus <@ %p ==> (%q = %r) = ((%p |&| %q) = (%p |&| %r)) @> [
    ident_implies_eq_and_eq p <@ %q = %r @> |> apply_left
    distrib_and_eq p q r |> apply_left
]

let ``3.71`` = theorem prop_calculus <@ %p ==> %p @> [
    def_implies |> apply
]



let ``3.76c`` = theorem prop_calculus <@ %p |&| %q ==> %p ||| %q @> [
    def_implies |> apply_left
    commute |> apply_left |> after_left
    distrib |> apply_left |> after_left
    commute |> apply
    idemp_or p |> apply
    distrib |> apply_left |> after_right
    idemp_and p |> apply
    distrib |> apply
    distrib |> apply_right |> after_left
    distrib |> apply_left
    idemp_or p |> apply_left
    distrib |> apply_left
    commute_or q p |> apply_left
    idemp_and <@ %p ||| %q @> |> apply_left
    commute |> apply_left |> after_left
    distrib |> apply_left |> after_left
    idemp_and q |> apply_left
    absorb_or q p |> CommuteL |> apply_left
    commute |> apply_right |> after_left
    left_assoc |> apply_left
    idemp_or q |> apply_left
] 

theorem prop_calculus <@  (%p |&| %q ) ==> %p @> [
    def_implies |> apply
    commute |> apply_left
    absorb_or p q |> Lemma'
]

theorem prop_calculus <@ %p ==> true @> [
    def_implies |> apply
    zero_or p |> apply_left
    ident_eq <@ true @> |> apply_left
]

let ``3.76d`` = theorem prop_calculus <@ (%p ||| (%q |&| %r)) ==> (%p ||| %q) @> [
    distrib |> apply_left
    strengthen_and <@ %p ||| %q @> <@ %p ||| %r @> |> Lemma
]

let ``3.76e`` = theorem prop_calculus <@ (%p |&| %q)  ==> (%p |&| (%q ||| %r)) @> [
    distrib |> apply_right
    weaken_or <@ %p |&| %q @> <@ %p |&| %r @> |> Lemma
]

let ``3.77`` = theorem prop_calculus <@ %p |&| (%p ==> %q) ==> %q @> [
    ident_and_implies p q |> L
    commute_and p q |> LR
    strengthen_and q p |> Lemma 
]

let ``3.78`` = theorem prop_calculus <@ (%p ==> %r) |&| (%q ==> %r) = (%p ||| %q  ==> %r) @> [
    ident_implies_not_or <@ %p ||| %q @> r |> apply_right
    distrib|> apply_left |> after_right
    distrib_or_and r <@ not %p @> <@ not %q@> |> CommuteL |> apply_right
    commute |> apply_left |> after_right
    commute |> apply_right |> after_right
    ident_implies_not_or p r |> Commute |> apply_right
    ident_implies_not_or q r |> Commute |> apply_right
]

let ``3.79`` = theorem prop_calculus <@ (%p ==> %r) |&| (not %p ==> %r ) = %r @> [
    case_analysis_1 p <@ not %p @> r |> apply
    excluded_middle |> apply_left |> after_left
    ident_conseq_true r |> Lemma'
]

let ``3.81`` = theorem prop_calculus <@ (%p ==> %q) |&| (%q ==> %p) ==> (%p = %q) @> [
  mutual_implication' p q |> apply_left  
  reflex_implies <@ %p = %q @> |> Lemma
] 

let ``3.82b`` = theorem prop_calculus <@ (%p = %q) |&| (%q ==> %r) ==> (%p ==> %r) @> [
    mutual_implication' p q |> Commute |> apply_left
    rshunt |> apply
    commute |> apply_left
    left_assoc |> apply_left
    left_assoc |> apply_left |> after_left
    ident_and_implies p q |> apply_left |> after_left
    right_assoc |> apply_left |> after_left
    ident_and_implies q p |> apply_left |> after_left
    commute |> apply_right |> after_left |> after_left
    left_assoc |> apply_left |> after_left
    idemp_and p |> apply_left |> after_left
    right_assoc |> apply_left
    ident_and_implies q r |> apply_left
    left_assoc |> apply_left
    commute |> apply_left
    strengthen_and r <@ %p |&| %q @> |> Lemma
]

let ``3.84`` = theorem prop_calculus <@ %p |&| %q ==> %r = (%p ==> (%q ==> %r)) @> [
    ident_implies_eq_and_eq <@ %p |&| %q @> r |> apply_left
    ident_implies_eq_and_eq q r |> apply_right
    ident_implies_eq_and_eq p <@ %q |&| %r = %q @> |> apply_right
    distrib_and_eq p <@ %q |&| %r @> q |> apply_right
    left_assoc_and p q r |> apply_right
    right_assoc |> apply_right
    def_true p |> Commute |> apply_right
    left_assoc |> apply
    commute |> apply
]

let ``3.84a`` = theorem prop_calculus <@ %p |&| %q ==> (%p |&| (%q ||| %r)) @> [
    subst_true |> apply
    commute |> apply_left
    subst_true |> apply
    distrib |> apply_right
    idemp_and <@ true @> |> apply_left |> after_right
    zero_or <@ true |&| %r @> |> CommuteL |> apply_right
    implies_true <@ %q |&| %p @> |> Lemma
]

let ``3.84b`` = theorem prop_calculus <@ %p ==> (%q ==> %p) @> [
    def_implies |> apply_right
    def_implies |> apply
    commute |> apply_left |> after_right |> after_left 
    distrib |> apply_left
    left_assoc |> apply_left |> after_left
    idemp_or p |> apply_left |> after_left |> after_left
    commute |> apply_left |> after_left 
    idemp_or p |> apply_right |> after_left
] 

let ``4.1`` = theorem prop_calculus <@ (%p ==> %q) ==> ((%p ||| %r) ==> (%q ||| %r)) @> [
    def_implies |> apply_right
    commute_or_or p r q r |> apply_left |> after_right
    idemp_or r |> apply_left |> after_right
    commute_or <@ %p ||| %q @> r |> apply_left |> after_right
    commute_or q r |> apply_right |> after_right
    collect_or_eq r <@ %p ||| %q @> q |> R
    commute |> apply_right
    def_implies' p q |> Commute |> apply_right
    weaken_or <@ %p ==> %q @> r |> Lemma
]

let ``4.4a`` = theorem prop_calculus <@ %p |&| %q ==> (%p = %q) @> [
    let lemma1 = lemma prop_calculus <@ %p |&| %q ==> %p @> [
        strengthen_and p q |> Lemma
    ]
    let lemma2 = lemma prop_calculus <@ %p |&| %q ==> %q @> [
        commute |> apply_left
        strengthen_and q p |> Lemma
    ]
    Deduce lemma1 |> apply_right
    Deduce lemma2 |> apply_right
    def_true <@ true @> |> Commute |> apply_right
]

let ``4.4b`` = theorem prop_calculus <@ (%p ==> %r) ==> ((%q ==> %s) ==> (%p |&| %q ==> (%r |&| %s))) @> [
    shunt' <@ %p ==> %r @> <@ %q ==> %s @> <@ (%p |&| %q ==> (%r |&| %s)) @> |> Commute |> apply
    let lemma1 = lemma prop_calculus <@ (%p ==> %r) ==> (%p = (%p |&| %r)) @> [
        commute |> apply_right
        ident_implies_eq_and_eq p r |> apply_left
    ]
    let lemma2 = lemma prop_calculus <@ (%q ==> %s) ==> (%q = (%q |&| %s)) @> [
        commute |> apply_right
        ident_implies_eq_and_eq q s |> apply_left
    ]
    Deduce' lemma1 |> apply_right
    Deduce' lemma2 |> apply_right
    right_assoc_and p r <@ %q |&| %s @> |> apply_right
    left_assoc_and r q s|>  apply_right
    commute_and r q |> apply_right
    right_assoc_and q r s |> apply_right
    left_assoc_and p q <@ %r |&| %s @> |> apply_right   
    commute |> apply_left |> after_right
    strengthen_and <@ %r |&| %s @> <@ %p |&| %q @> |> Taut |> apply_right
]
*)