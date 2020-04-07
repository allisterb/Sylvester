namespace Sylvester

open FSharp.Quotations

/// Propositional calculus using the axioms and rules of S.
module PropCalculus =
    let prop_calculus = Theory.S

    /// Reduce logical constants in expression. 
    let Reduce = Theory.S.Rules.[0]

    /// Reduce logical identity terms in expression. 
    let ReduceIdent = Theory.S.Rules.[1]

    /// Logical expression is left associative.
    let LeftAssoc = Theory.S.Rules.[2]

    /// Logical expression is right associative.
    let RightAssoc = Theory.S.Rules.[3]
  
    /// Logical expression is commutative.
    let Commute = Theory.S.Rules.[4]

    /// Distribute logical terms in expression.
    let Distrib = Theory.S.Rules.[5]

    /// Collect distributed logical terms in expression.
    let Collect = Theory.S.Rules.[6]

    /// Logical operators are idempotent.
    let Idemp = Theory.S.Rules.[7]

    /// Logical expression satisfies law of excluded middle.
    let ExcludedMiddle = Theory.S.Rules.[8]

    /// Logical expression satisfies golden rule.
    let GoldenRule = Theory.S.Rules.[9]

    let reduce_constants = EquationalLogic.reduce_constants

    let reduce_ident = EquationalLogic.reduce_ident

    let left_assoc = EquationalLogic.left_assoc

    let right_assoc = EquationalLogic.right_assoc

    let commute = EquationalLogic.commute

    let distrib = EquationalLogic.distrib

    let collect = EquationalLogic.collect

    let idemp = EquationalLogic.idemp

    let excluded_middle = EquationalLogic.excluded_middle

    let golden_rule = EquationalLogic.golden_rule

    (* proof step shortcuts *)
    let eq_id_ax expr = id_ax prop_calculus expr
    let eq_id_ax_lr expr = id_ax_lr prop_calculus expr
    let eq_id_ax_l expr = id_ax_l prop_calculus expr
    let eq_id_ax_r expr = id_ax_r prop_calculus expr

    let eq_id steps expr = ident prop_calculus steps expr 
    let eq_id_lr steps expr = id_lr prop_calculus steps expr
    let eq_id_l steps expr= id_l prop_calculus steps expr
    let eq_id_r steps expr = id_r prop_calculus steps expr

    (* Parameterized rules *)
    let CommuteWith p q = eq_id_ax <@ (%p ||| %q) = (%q ||| %p)@>

    let LeftAssocWith p q r = eq_id <@ (%p ||| (%q ||| %r)) = ((%p ||| %q) ||| %r) @> [LR LeftAssoc]

    let RightAssocWith p q r = eq_id_ax <@ ((%p ||| %q) ||| %r) = (%p ||| (%q ||| %r)) @>

    (* Tactics *)

    /// Switch the LHS of an identity with the RHS.
    let Transpose = Tactics.Transpose Commute

    /// A theorem T is equivalent to T = true
    let Taut = Tactics.Taut ReduceIdent

    (* Additional theorems of S useful in proofs. *)
    
    /// true = (p = p)
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

    /// p ||| true = true
    let TrueZero p = ident prop_calculus <@ %p ||| true = true @> [
        True p |> LR 
        LR Distrib
        LR Idemp
    ]

    /// p ||| false = p
    let FalseIdent p = ident prop_calculus <@ %p ||| false = %p @> [
        False p |> LR
        L Distrib
        LR RightAssoc
        R Idemp
        True p |> R
        L ExcludedMiddle
    ]