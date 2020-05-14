#load "Include.fsx"

open Sylvester
open PropCalculus

/// true = (p = p)
let p,q,r,s = var4<bool>
let p',q',r',s' = <@ p @>, <@ q @>, <@ r @>, <@ s @>

let ``3.52`` = proof prop_calculus <@ (p = q) = (p |&| q) ||| (not p |&| not q) @> [
    collect |> R
    commute |> L |> L'
    commute |> L
    commute |> R |> L'
    golden_rule p' q' |> LeftAssoc |> L
]
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


let ``3.76a`` = proof prop_calculus <@ p ==> (p ||| q) @> [
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

proof prop_calculus <@  (p |&| q ) ==> p @> [
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

let ``3.77`` = proof prop_calculus <@ p |&| (p ==> q) ==> q @> [
    ident_and_implies p' q' |> L
    commute_and p' q' |> LR
    weaken_and q' p' |> Taut' |> LR
]

let ``3.78`` = proof prop_calculus <@( p ==> r) |&| (q ==> r) = (p ||| q  ==> r) @> [
    ident_implies_not_or <@p ||| q@> r' |> R
    distrib|> L |> R'
    distrib_or_and r' <@ not p @> <@ not q@> |> CommuteL |> R
    commute |> L |> R'
    commute |> R |> R'
    ident_implies_not_or p' r' |> Commute |> R
    ident_implies_not_or q' r' |> Commute |> R
]

let ``3.79`` = proof prop_calculus <@ (p ==> r) |&| (not p ==> r ) = r @> [
    case_analysis_1 p' <@not p @> r' |> LR
    excluded_middle' |> L |> L'
    ident_conseq_true r' |> Lemma
]

let ``3.80`` = proof prop_calculus <@ ((p ==> q) |&| (q ==> p)) = p = q @> [
    right_assoc |> LR
    ident_implies_not_or p' q' |> L
    ident_implies_not_or q' p' |> L  
    distrib |> L  
    commute |> L |> L' |> L'
    commute |> R |> L'
    distrib |> L |> L'
    distrib |> R |> L'
    distrib |> L |> L' |> L'
    commute |> L |> L'
    distrib |> L |> L'
    contr q' |> CommuteL |> L
    contr p' |> L
    ident_or <@ p |&| q @> |> CommuteL |> L
    ident_or <@ not q |&| not p @> |> CommuteL |> L
    commute |> L |> L'
    ident_eq_and_or_and p' q' |> Commute |> Lemma
]