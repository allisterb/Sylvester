namespace Sylvester

open FSharp.Quotations

/// Propositional calculus using the axioms and rules of S.
module PropCalculus =
    let prop_calculus = Theory.S

    /// Reduce logical constants in expression. 
    let Reduce = S.Rules.[0]

    /// Logical expression is left associative.
    let LeftAssoc = S.Rules.[1]

    /// Logical expression is right associative.
    let RightAssoc = S.Rules.[2]
  
    /// Logical expression is commutative.
    let Commute = S.Rules.[3]

    /// Distribute logical terms in expression.
    let Distrib = S.Rules.[4]

    /// Collect distributed logical terms in expression.
    let Collect = S.Rules.[5]

    /// Logical operators are idempotent.
    let Idemp = S.Rules.[6]

    /// Logical expression satisfies law of excluded middle.
    let ExcludedMiddle = Theory.S.Rules.[7]

    /// Logical expression satisfies golden rule.
    let GoldenRule = Theory.S.Rules.[8]

    (* proof step shortcuts *)
    
    let eq_id_ax expr = id_ax prop_calculus expr
    let eq_id_ax_lr expr = id_ax_lr prop_calculus expr
    let eq_id_ax_l expr = id_ax_l prop_calculus expr
    let eq_id_ax_r expr = id_ax_r prop_calculus expr

    let eq_id expr = ident prop_calculus expr 
    let eq_id_lr expr = id_ax_lr prop_calculus expr
    let eq_id_l expr proof = id_l prop_calculus proof expr
    let eq_id_r expr proof = id_r prop_calculus proof expr

    (* Additional theorems of S useful in proofs. *)

    /// p == p == true
    let TruthDefn (p:Expr<bool>) = <@ (%p == %p) == true @> |> id_ax prop_calculus 

    // not p = p = false
    let FalseDefn (p:Expr<bool>) = <@(not %p == %p) == false@> |> ident prop_calculus [
            Collect |> L
            <@(%p = %p) = true @> |> eq_id_ax_l
        ]
        
    /// not p = q = p = not q
    let NotEquivSymmetry (p:Expr<bool>) (q:Expr<bool>) = <@ not %p = %q = %p = not %q @> |> lemma prop_calculus [
            Collect |> L
            RightAssoc |> LR
            Commute |> R
            Collect |> R
            Commute |> R
        ] 
        
    // not not p == p
    let DoubleNegation (p:Expr<bool>) = <@ not (not %p) = %p @> |> lemma prop_calculus [
            Collect |> L
            FalseDefn p |> L
        ]
       