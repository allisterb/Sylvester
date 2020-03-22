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

    //Short cuts
    let eq_id_ax_a expr = id_ax_a S expr
    let eq_id_ax_b expr = id_ax_b S expr
    
    let eq_id_a expr proof = id_a S proof expr
    let eq_id_b expr proof = id_b S proof expr

    let eq_id_ax_r_a expr = id_ax_r_a S expr
    let eq_id_ax_r_b expr = id_ax_r_b S expr

    let eq_id_ax_l_a expr = id_ax_l_a S expr
    let eq_id_ax_l_b expr = id_ax_l_b S expr

    // Additional theorems of S useful in proofs.

    /// p = p = true
    let TruthDefn (p:Expr<bool>) = <@ (%p = %p) = true @> |> ident_axiom prop_calculus |> Lemma

    // not p = p = false
    let FalseDefn (p:Expr<bool>) = 
        let t = <@not %p = %p @> |> contr prop_calculus [
            Collect |> EntireA
            <@(%p = %p) = true @> |> ident_axiom prop_calculus |> Lemma |> EntireA
        ]
        Lemma t

    /// not p = q = p = not q
    let NotEquivSymmetry (p:Expr<bool>) (q:Expr<bool>) = 
        let t = <@ not %p = %q = %p = not %q @> |> theorem S [
            Collect |> LeftA
            RightAssoc |> EntireA
            Commute |> RightA
            Collect |> RightA
            Commute |> RightA
        ] 
        Lemma t
    
    // not not p == p
    // Theorem 3.12 
    let DoubleNegation (p:Expr<bool>) =
        let t = <@ not (not %p) = %p @> |> theorem prop_calculus [
            Collect |> EntireA
            FalseDefn p |> EntireA
        ]
        Lemma t