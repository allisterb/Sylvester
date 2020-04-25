#load "Include.fsx"

open Sylvester
open PropCalculus

/// true = (p = p)
let p,q,r = var3<bool>
/// 
let DefTrue p = eq_id_ax <@ true = (%p = %p) @>  

/// false = (not p = p)
let DefFalse p = eq_id <@ false = (not %p = %p) @> [
 LR Collect
 DefTrue p |> Transpose |> R
] 

/// not false = true
let NotFalse = ident prop_calculus <@not false = true@> [
    LR Commute
    DefTrue <@ false @> |> L
    LR RightAssoc
    R Commute
    R Collect
    DefTrue <@ false @> |> Transpose |> R 
]

/// not not p = p

let DoubleNegation p = ident prop_calculus <@not (not %p) = %p @> [
     LR Collect
     DefFalse p |> Transpose |> LR
 ]

/// not p = q = p = not q
let NotEquivSymm p q = ident prop_calculus <@ not %p = %q = %p = not %q @> [
     Collect |> L
     RightAssoc |> LR
     Commute |> R
     Collect |> R
     Commute |> R
]

/// not p = q = p = not q
let GoldenRule1 p q = ident prop_calculus <@ (%p |&| %q) = ((%p = %q) = (%p ||| %q)) @> []

let DisjZero p = proof prop_calculus <@ %p ||| true = true @> [
    DefTrue p |> LR 
    LR Distrib
    LR Idemp
]

let ConjIdent p = proof prop_calculus <@ %p ||| false = %p @> [
    DefFalse p |> LR
    L Distrib
    LR RightAssoc
    R Idemp
    DefTrue p |> R
    L ExcludedMiddle
]

let DefOr2 = ident prop_calculus <@ (p ||| q) = (p ||| not q = p) @> [
    LR LeftAssoc
    CollectOrEq <@ p @> <@ q @> <@ not q @> |> L
    CommuteEq <@ q @> <@ not q @> |> L
    DefFalse <@ q @> |> Transpose |> L
    IdentOr <@ p @> |> L      
]
