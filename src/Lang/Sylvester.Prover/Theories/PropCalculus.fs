namespace Sylvester

open FSharp.Quotations

open Patterns

/// Propositional calculus using the axioms and rules of S.
module PropCalculus =
    let prop_calculus = Theory.S

    /// Reduce logical constants in expression. 
    let Reduce = Theory.S.Rules.[0]

    /// Logical expression is left associative.
    let LeftAssoc = Theory.S.Rules.[1]

    /// Logical expression is right associative.
    let RightAssoc = Theory.S.Rules.[2]
  
    /// Logical expression is commutative.
    let Commute = Theory.S.Rules.[3]

    /// Distribute logical terms in expression.
    let Distrib = Theory.S.Rules.[4]

    /// Collect distributed logical terms in expression.
    let Collect = Theory.S.Rules.[5]

    /// Logical operators are idempotent.
    let Idemp = Theory.S.Rules.[6]

    /// Logical expression satisfies law of excluded middle.
    let ExcludedMiddle = Theory.S.Rules.[7]

    /// Logical expression satisfies golden rule.
    let GoldenRule = Theory.S.Rules.[8]

    let reduce_constants = EquationalLogic.reduce_constants

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

    (* Tactics *)
    /// Switch the LHS of an identity with the RHS
    let SwitchLR = Tactics.SwitchLR Commute

    (* Additional theorems of S useful in proofs. *)
    
    /// true = (p = p)
    let True p = eq_id_ax <@ true = (%p = %p) @>  

    let False p = eq_id <@ false = (not %p = %p) @> [
     LR Collect
     True <@ %p @> |> Tactics.SwitchLR Commute |> R
    ] 

    /// not false = true
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
 
    /// not not p = p
    let DoubleNegation p = ident prop_calculus <@not (not %p) = %p @> [
         LR Collect
         False <@ %p @> |> SwitchLR |> LR
     ]

    /// not p = q = p = not q
    let NotEquivSymm p q = ident prop_calculus <@ not %p = %q = %p = not %q @> [
         Collect |> L
         RightAssoc |> LR
         Commute |> R
         Collect |> R
         Commute |> R
    ]