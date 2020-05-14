#load "Include.fsx"

open Sylvester
open PropCalculus

/// true = (p = p)
let p,q,r,s = var4<bool>
let p',q',r',s' = <@ p @>, <@ q @>, <@ r @>, <@ s @>

let ``3.59`` = proof prop_calculus <@ p ==> q = (not p ||| q) @> [
    def_implies' |> L
    ident_or_not_or q' p' |> CommuteL |> R
    commute |> R
    commute |> L |> R'
]

let ``3.60`` = proof prop_calculus <@ p ==> q = ((p |&| q) = p) @> [
    def_implies' |> L
    commute |> LR
    right_assoc |> LR
    commute |> R |> R' 
    left_assoc |> R 
]

let ``3.61`` = proof prop_calculus <@ p ==> q = (not q ==> not p) @> [
    def_implies' |> R
    commute |> R
    commute |> R |> R'
    distrib_not_and p' q' |> Commute |> R |> R'
    symm_eq_not_eq p' <@ p |&| q @> |> Commute |> R 
    commute |> R
    ident_implies_eq_and_eq p' q' |> Lemma
]

let ``3.62`` = proof prop_calculus <@p ==> (q = r) = ((p |&| q) = (p |&| r))@> [
    ident_implies_eq_and_eq p' <@ q = r @> |> L
    distrib_and_eq p' q' r' |> L
]


let ``3.71`` = proof prop_calculus <@ p ==> p @> [
    def_implies' |> LR
]

let ``3.76a`` = ident prop_calculus <@ p ==> (p ||| q) @> [
    def_implies' |> LR
    left_assoc |> L
    idemp_or p' |> L
]

let ``3.76c`` = proof prop_calculus <@ p |&| q ==> p ||| q @> [
    def_implies' |> L
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

let proof prop_calculus <@  (p |&| q ) ==> p@> [
    def_implies' |> LR
    commute |> L
    absorb_or p' q' |> Lemma
]

proof prop_calculus <@ p ==> true @> [
    def_implies' |> LR
    zero_or p' |> L
    //ident_eq <@ true @> |> L
]

let ``3.76d`` = proof prop_calculus <@ (p ||| (q |&| r)) ==> (p ||| q) @> [
    distrib |> L
    weaken_and <@ p ||| q @> <@ p ||| r @> |> Lemma'
]

let ``3.76e`` = proof prop_calculus <@ (p |&| q)  ==> (p |&| (q ||| r)) @> [
    distrib |> R
    weaken <@ p |&| q @> <@ p |&| r @> |> Lemma'
]

(*
let ``3.63`` = proof prop_calculus <@p ==> (q = r) = ((p ==> q) = (p ==> r))@> [
    distrib_implies_eq_implies p' q' r' |> L
    ident_implies_eq_and_eq p' q' |> L |> R'
    ident_implies_eq_and_eq p' r' |> R |> R'
    commute |> R |> R'
    left_assoc |> R 
    right_assoc |> L |> R'
    def_true p' |> Commute |> R
    ident_eq <@ p |&| q @> |> L |> R'
]

let ``3.66`` = proof prop_calculus <@ p |&| (p ==> q) = (p |&| q) @> [
    ident_implies_eq_and_eq p' q' |> L
    distrib_and_eq p' <@ p |&| q @> p' |> L
    left_assoc |> L |> L' |> L'
    idemp_and p' |> L 
]

let ``3.68`` = proof prop_calculus <@ (p ||| (p ==> q)) = true @> [
    def_implies' |> R |> L'
    distrib |> L
    left_assoc |> L |> L'
    idemp_or p' |> L
    ident_eq <@ (p ||| q) = (p ||| q) @> |> LR
]

let ``3.75``  = proof prop_calculus <@ (false ==> p) = true @> [
    def_implies' |> L
    ident_or p' |> CommuteL |> L
    commute |> LR
]

let ``3.66a`` = proof prop_calculus <@ p ==> (p ||| q) @> [
    def_implies' |> LR
    left_assoc |> L
    idemp_or p' |> L
]

let ``3.76b`` = proof prop_calculus <@ p ||| (p ==> q) = (p |&| q) @> [
    ident_implies_eq_and_eq p' q' |> L
    distrib_and_eq p' <@ p |&| q @> p' |> L
    left_assoc |> L |> L' |> L'
    idemp_and p' |> L 
]

let ``3.71`` = proof prop_calculus <@ (p ==> p) @> [
    def_implies' |> LR
]
*)