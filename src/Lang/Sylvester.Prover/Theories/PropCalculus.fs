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
    let eq_id_ax_ab expr = id_ax_ab prop_calculus expr
    let eq_id_ax_a expr = id_ax_a prop_calculus expr
    let eq_id_ax_b expr = id_ax_b prop_calculus expr
    let eq_id_ax_r_a expr = id_ax_r_a prop_calculus expr
    let eq_id_ax_r_b expr = id_ax_r_b prop_calculus expr
    let eq_id_ax_l_a expr = id_ax_l_a prop_calculus expr
    let eq_id_ax_l_b expr = id_ax_l_b prop_calculus expr

    let eq_id expr = ident expr prop_calculus 
    let eq_id_ab expr = id_ax_ab prop_calculus expr
    let eq_id_a expr proof = id_a prop_calculus proof expr
    let eq_id_b expr proof = id_b prop_calculus proof expr
    let eq_id_r_a expr = id_r_a prop_calculus expr
    let eq_id_r_b expr = id_r_b prop_calculus expr
    let eq_id_l_a expr = id_l_a prop_calculus expr
    let eq_id_l_b expr = id_l_b prop_calculus expr

    (* Additional theorems of S useful in proofs. *)

    /// p = p = true
    let TruthDefn (p:Expr<bool>) = <@ (%p = %p) = true @> |> id_ax prop_calculus 

    // not p = p = false
    let FalseDefn (p:Expr<bool>) = <@not %p = %p @> |> contr_lem prop_calculus [
            Collect |> EntireA
            <@(%p = %p) = true @> |> eq_id_ax_a
        ]
        
    /// not p = q = p = not q
    let NotEquivSymmetry (p:Expr<bool>) (q:Expr<bool>) = <@ not %p = %q = %p = not %q @> |> lemma prop_calculus [
            Collect |> LeftA
            RightAssoc |> EntireA
            Commute |> RightA
            Collect |> RightA
            Commute |> RightA
        ] 
        
    // not not p == p
    let DoubleNegation (p:Expr<bool>) = <@ not (not %p) = %p @> |> lemma prop_calculus [
            Collect |> EntireA
            FalseDefn p |> EntireA
        ]
       