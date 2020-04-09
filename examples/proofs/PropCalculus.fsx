#load "Include.fsx"

open Sylvester
open PropCalculus

/// true = (p = p)
let p,q,r = var3<bool>
/// 
let True p = eq_id_ax <@ true = (%p = %p) @>  

/// false = (not p = p)
let False p = eq_id <@ false = (not %p = %p) @> [
 LR Collect
 True p |> Transpose |> R
] 

/// not false = true
let NotFalse = ident prop_calculus <@not false = true@> [
    LR Commute
    True <@ false @> |> L
    LR RightAssoc
    R Commute
    R Collect
    True <@ false @> |> Transpose |> R 
]

/// not not p = p
let DoubleNegation p = ident prop_calculus <@not (not %p) = %p @> [
     LR Collect
     False p |> Transpose |> LR
 ]

/// not p = q = p = not q
let NotEquivSymm p q = ident prop_calculus <@ not %p = %q = %p = not %q @> [
     Collect |> L
     RightAssoc |> LR
     Commute |> R
     Collect |> R
     Commute |> R
]

let Zero p = proof prop_calculus <@ %p ||| true = true @> [
    True p |> LR 
    LR Distrib
    LR Idemp
]

let FalseIdent p = proof prop_calculus <@ %p ||| false = %p @> [
    False p |> LR
    L Distrib
    LR RightAssoc
    R Idemp
    True p |> R
    L ExcludedMiddle
]

let OrDistrib p q r = proof prop_calculus <@ (%p ||| (%q ||| %r)) = ((%p ||| %q) ||| (%p ||| %r)) @> [
    CommuteWith p q |> R
    R RightAssoc
    LeftAssocWith p p r |> R
    R Idemp
    R LeftAssoc
    L LeftAssoc
    CommuteWith p q |> L
]

OrDistrib <@ p @> <@ q @> <@ r @> |> last_state |> expand_right |>  src


/// not p = q = p = not q
//let GoldenRule1 p q = ident prop_calculus <@ (%p |&| %q) = ((%p = %q) = (%p ||| %q)) @> [
     
//]
//let p = false
//GoldenRule1 <@ p @> <@ q @> 

//(p |&| q) ||| q