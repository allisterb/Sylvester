#load "Include.fsx"

open Sylvester
open PropCalculus

let True p = eq_id_ax <@ true = (%p = %p) @>  
   
let False p = eq_id <@ false = (not %p = %p) @> [
    LR Collect
    True <@ %p @> |> SwitchLR |> R
] 

// not false = true
let NotFalse = 
    let stmt = <@not false = true@>
    let lemma1 = eq_id <@ true = (false = false) @> [
        LR Commute
        True <@ false @> |> SwitchLR |> L
    ]
    let lemma2 = eq_id <@ (false = false) = true @> [
        True <@ false @> |> SwitchLR  |> L
    ]
    ident prop_calculus stmt [
        LR Commute
        L lemma1
        LR RightAssoc
        R Commute
        R Collect
        R lemma2 
    ]
    
let DoubleNegation p = ident prop_calculus <@not (not %p) = %p @> [
        LR Collect
        False <@ %p @> |> SwitchLR |> LR
    ]

/// not p = q = p = not q
let NotEquivSymmetry p q = ident prop_calculus <@ not %p = %q = %p = not %q @> [
    Collect |> L
    RightAssoc |> LR
    Commute |> R
    Collect |> R
    Commute |> R
]

let p = false
DoubleNegation <@ p @> 
