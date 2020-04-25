namespace Sylvester

open FSharp.Quotations

/// Propositional calculus using the axioms and rules of S.
module PropCalculus =
    let prop_calculus = Theory.S

    (* Expression tree functions used in rules *)
    
    let reduce_constants = EquationalLogic.reduce_constants

    let left_assoc = EquationalLogic.left_assoc

    let right_assoc = EquationalLogic.right_assoc

    let commute = EquationalLogic.commute

    let distrib = EquationalLogic.distrib

    let collect = EquationalLogic.collect

    let idemp = EquationalLogic.idemp

    let excluded_middle = EquationalLogic.excluded_middle

    let golden_rule = EquationalLogic.golden_rule

    (* Admissible rules *)

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

    (* proof step shortcuts *)
    
    let eq_id steps expr = ident prop_calculus steps expr
    let eq_id_ax expr = id_ax prop_calculus expr
     
    (* Additional theorems of S useful in proofs *)
    
    /// p = q = q = p
    let CommuteEq p q = ident prop_calculus <@ (%p = %q) = (%q = %p) @> [LR LeftAssoc]

    /// p ||| q = q ||| p
    let CommuteOr p q = id_ax prop_calculus <@ (%p ||| %q) = (%q ||| %p) @>

    /// p |&| q = q |&| p
    let CommuteAnd p q = ident prop_calculus <@ (%p |&| %q) = (%q |&| %p) @> [
        LR GoldenRule
        CommuteOr <@ %q @> <@ %p @> |> R
        CommuteEq <@ %q @> <@ %p @> |> R 
    ]
 
    /// p ||| (q ||| r) = p ||| q ||| r
    let LeftAssocOr p q r = id_ax prop_calculus <@ (%p ||| (%q ||| %r)) = ((%p ||| %q) ||| %r) @>

    /// (p ||| q) ||| r = p ||| (q ||| r)
    let RightAssocOr p q r = id_ax prop_calculus <@ ((%p ||| %q) ||| %r) = (%p ||| (%q ||| %r)) @>

    /// p = (q = r) = p = q = r
    let LeftAssocEq p q r = ident prop_calculus <@ (%p = (%q = %r)) = ((%p = %q) = %r) @> [R RightAssoc]

    /// (p = q) = r = p = (q = r)
    let RightAssocEq p q r = id_ax prop_calculus <@ ((%p = %q) = %r) = (%p = (%q = %r)) @>

    /// p ||| (q ||| r) = (p ||| q) = (p ||| r)
    let DistribOr p q r = id_ax prop_calculus <@ (%p ||| (%q = %r)) = ((%p ||| %q) = (%p ||| %r)) @>

    (* Tactics *)

    /// Switch the LHS of an identity with the RHS.
    let Transpose = Tactics.Transpose Commute


    /// A theorem T is equivalent to T = true
    //let Taut = Tactics.Taut ReduceIdent

    /// (p ||| q) = (p ||| r) = p ||| (q ||| r)
    let CollectOr p q r = DistribOr p q r |> Transpose
    
    /// true = (p = p)
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
    let DoubleNegation p = ident prop_calculus <@(not (not %p)) = %p @> [
         LR Collect
         DefFalse p |> Transpose |> LR
    ]

    /// not p = q = p = not q
    let SymmNotEquiv p q = ident prop_calculus <@ not %p = %q = %p = not %q @> [
         Collect |> L
         RightAssoc |> LR
         Commute |> R
         Collect |> R
         Commute |> R
    ]

    /// p ||| true = true
    let ZeroOr p = ident prop_calculus <@ (%p ||| true) = true @> [
        DefTrue p |> LR 
        LR Distrib
        LR Idemp
    ]

    /// p ||| false = p
    let IdentOr p = ident prop_calculus <@ (%p ||| false) = %p @> [
        DefFalse p |> LR
        L Distrib
        LR RightAssoc
        R Idemp
        DefTrue p |> R
        L ExcludedMiddle
    ]

    /// p |&| not p = false
    let Contr p = ident prop_calculus <@ %p |&| not %p = false@> [
        GoldenRule |> L
        ExcludedMiddle |> L
        CommuteEq <@ %p @> <@ not %p @> |> L
        DefFalse <@ %p @> |> Transpose |> L
        CommuteEq <@ false @> <@ true @> |> L
        RightAssoc |> LR
    ]

    /// not (p = q) = not p = q
    let DistribNot p q = ident prop_calculus <@ (not (%p = %q)) = (not %p = %q) @> [LR RightAssoc]

    /// (not p = q) = not (p = q) 
    let CollectNot p q = DistribNot p q |> Transpose

    /// p <> q = not (p = q)
    let DefNotEquiv p q = ident prop_calculus <@ (%p <> %q) = (not (%p = %q)) @> [
        RightAssoc |> LR
    ]

    /// p <> q = q <> p
    let CommuteNotEquiv p q = ident prop_calculus <@ (%p <> %q) = (%q <> %p) @> [
        id_ax prop_calculus <@ (%p <> %q) = not (%p = %q) @> |> L
        id_ax prop_calculus <@ (%q <> %p) = not (%q = %p) @> |> R
        R Commute
    ]

    /// (p <> q) <> r = p <> (q <> r)
    let RightAssocNotEquiv p q r = ident prop_calculus <@ ((%p <> %q) <> %r) = (%p <> (%q <> %r)) @> [
        DefNotEquiv <@ %p @> <@ %q @> |> L
        DefNotEquiv <@ not (%p = %q) @> <@ %r @> |> L
        DefNotEquiv <@ %q @> <@ %r @> |> R
        DefNotEquiv <@ %p @> <@ not (%q = %r) @> |> R
        DistribNot <@ %q @> <@ %r @> |> R
        LeftAssoc |> R
        CommuteEq <@ %p @> <@ not %q @> |> R
        CollectNot <@ %q @> <@ %p @> |> R
        CommuteEq <@ %q @> <@ %p @> |> R
    ]

    /// p <> (q <> r) = (p <> q) <> r  
    let LeftAssocNotEquiv p q r = RightAssocNotEquiv p q r |> Transpose