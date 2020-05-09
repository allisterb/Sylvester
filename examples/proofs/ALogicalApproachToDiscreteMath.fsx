#load "Include.fsx"

open Sylvester
open PropCalculus

let p,q,r = var3<bool>

//proof prop_calculus <@(true = true) = true@> [LR Commute]
let ``3.12`` = proof prop_calculus <@ not (not p) = p @> [
        Collect |> LR
        def_false <@ p @> |> Trn |> LR
        not_false |> Truth |> LR
        
        //def_true <@ p @> |> LR
]
let ``3.31`` = proof prop_calculus <@ (p ||| (q ||| r)) = ((p ||| q) ||| (p ||| r)) @> [
    idemp_or <@ p @> |> Trn |> L
    LeftAssoc |> L
    LeftAssoc |> R

]
let ``3.36`` = proof prop_calculus <@(p |&| q) = (q |&| p) @> [
    LR GoldenRule
    commute_or <@ q @> <@ p @> |> R
    commute_eq <@ q @> <@ p @> |> R 
]

let ``3.37`` = proof prop_calculus <@ (p |&| q |&| r) =  (p |&| (q |&| r)) @> [
    golden_rule <@ p @> <@ q @> |> L
    golden_rule <@ (p = q) = (p ||| q) @> <@ r @> |> L 
    commute_or <@ ((p = q) = (p ||| q)) @> <@ r @> |> L
    distrib_or_eq <@ r @> <@ p = q @> <@ p ||| q @> |> L
    distrib_or_eq <@ r @> <@ p @> <@ q @> |> L
    right_assoc_eq <@ p = q@> <@ p ||| q@> <@ r @> |> L
    commute_eq <@ p ||| q @> <@ r @> |> L
    commute_or <@ r @> <@ q @> |> L
    commute_eq <@ r ||| p@> <@ q ||| r@> |> L
    commute_or <@ r @> <@ p ||| q @> |> L
    L LeftAssoc
    L LeftAssoc
    L LeftAssoc
    commute_and <@ p @> <@ q |&| r @> |> R
    ident_and_eq_all <@ q @> <@ r @> <@ p @> |> R
    commute_eq <@q = r@> <@ p @> |> R
    left_assoc_eq <@ p @> <@ q @> <@ r @> |> R
    commute_or <@ q ||| r @> <@ p @> |> R
    left_assoc_or <@ p @> <@ q @> <@ r @> |> R
    right_assoc_eq <@ p = q = r @> <@ q ||| r @> <@ r ||| p @> |> R
    LeftAssoc |> R
    right_assoc_eq <@ p = q = r = (q ||| r) @> <@ r ||| p @> <@ p ||| q@> |> R
    commute_eq <@ ( r ||| p) @> <@ p ||| q @> |> R
    LeftAssoc |> R
    right_assoc_eq <@ p = q = r @> <@ q ||| r @> <@ p ||| q @> |> R
    commute_eq <@ q ||| r @> <@ p ||| q @> |> R
    LeftAssoc |> R
]

let ``3.43A`` = proof prop_calculus <@ (p |&| (p ||| q)) = p @> [
    GoldenRule |> L
    left_assoc_or <@ p @> <@ p @> <@ q @> |> L
    idemp_or <@ p @> |> L
]

let ``3.43B`` = proof prop_calculus <@ (p ||| (p |&| q)) = p @> [
    GoldenRule |> L
    Distrib |> L
    Distrib |> R
    left_assoc_or <@ p @> <@ p @> <@ q @> |> L
    idemp_or <@ p @> |> L
    Distrib |> L
    idemp_or <@ p @> |> L
]

let ``3.44b`` = proof prop_calculus <@ p ||| (not p |&| q) = (p ||| q) @> [
    GoldenRule |> L
    commute_or <@ not p @> <@ q @> |> L
    right_assoc_eq <@ not p @> <@ q @> <@ q ||| not p @> |> L
    ident_or_or_not <@ q @> <@ p @> |> Trn |> TrnL |> L
    Distrib |> L 
    commute_or <@ q @> <@ p @> |> L
    left_assoc_or <@ p @> <@ p @> <@ q @> |> L
    idemp_or <@ p @> |> L
    ExcludedMiddle |> L
    ident_eq <@ p ||| q @> |> TrnL |> L
] 

let ``3.45`` = proof prop_calculus <@ p ||| (q |&| r) = ((p ||| q) |&| (p ||| r)) @> [
    GoldenRule |> L
    GoldenRule |> R
    Distrib |> L
    Distrib |> L
    distrib_or <@ p @> <@ q @> <@ r @> |> L
]

let ``3.46`` = proof prop_calculus <@ not (p |&| q) = (not p ||| not q) @> [
    GoldenRule |> L
    Distrib |> L
    Distrib |> L 
    ident_or_or_not <@ not p @> <@ not q @> |> R
    double_negation <@ q @> |> R
    ident_or_not_or <@ q @> <@ p @> |> TrnL |> R
    Commute |> R
    commute_or <@ q @> <@ p @> |> R
]

let p347 = proof prop_calculus <@ p = q = (not p = not q) @> [
    LeftAssoc |> LR
    commute_eq <@ p = q @> <@ not p @> |> L
    commute_eq <@ p @> <@ q @> |> L
    LeftAssoc |> LR
    symm_not_eq <@ p @> <@ q @> |> Lemma
]

let ``3,47`` = proof prop_calculus <@ not (p ||| q) = (not p |&| not q) @> [
    golden_rule <@ p @> <@ q @> |> Trn |> TrnL |> RightAssoc' |> L
    Commute |> L
    Distrib |> L
    distrib_not_and <@ p @> <@ q @> |> L
    Commute |> LR
    symm_eq_not_eq <@ p @> <@ q @> |> R
    Commute |> R
]