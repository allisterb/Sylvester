namespace Sylvester

open FSharp.Quotations

/// Propositional calculus using the axioms and rules of S.
module PropCalculus =
    let prop_calculus = Theory.S

    (* Expression tree functions used in rules *)
    
    let _reduce_constants = EquationalLogic.reduce_constants

    let _left_assoc = EquationalLogic.left_assoc

    let _right_assoc = EquationalLogic.right_assoc

    let _commute = EquationalLogic.commute

    let _distrib = EquationalLogic.distrib

    let _collect = EquationalLogic.collect

    let _idemp = EquationalLogic.idemp

    let _excluded_middle = EquationalLogic.excluded_middle

    let _golden_rule = EquationalLogic.golden_rule

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
    
    let ppc_id steps expr = ident prop_calculus steps expr
    let ppc_id_ax expr = id_ax prop_calculus expr
     
    (* Tactics *)

    /// Switch the LHS of an identity with the RHS.
    let Trn = Tactics.Trn Commute
    
    /// If A is a theorem then the identity A = true is a theorem.
    let Taut =  
        let ieq p = Theorem(<@@ ((%%p) = true) = (%%p) @@>, Proof (<@@ (%%p = true) = %%p @@>, prop_calculus, [L Commute; LR RightAssoc], true)) |> Ident  
        Tactics.Taut ieq

    (* Additional theorems of S useful in proofs *)
    
    /// true = (p = p)
    let def_true p = id_ax prop_calculus <@ true = (%p = %p) @>  

    /// false = (not p = p)
    let def_false p = ident prop_calculus <@ false = (not %p = %p) @> [
        LR Collect
        def_true p |> Trn |> R
    ] 

    /// (p = true) = p
    let ident_eq p = ident prop_calculus <@ (%p = true) = %p @> [
        L Commute
        LR RightAssoc
    ]

    /// p = q = q = p
    let commute_eq p q = ident prop_calculus <@ (%p = %q) = (%q = %p) @> [LR LeftAssoc]

    /// p = (q = r) = p = q = r
    let left_assoc_eq p q r = ident prop_calculus <@ (%p = (%q = %r)) = ((%p = %q) = %r) @> [R RightAssoc]

    /// (p = q) = r = p = (q = r)
    let right_assoc_eq p q r = left_assoc_eq p q r |> Trn
    
    /// not false = true
    let not_false = ident prop_calculus <@not false = true@> [
        LR Commute
        def_true <@ false @> |> L
        LR RightAssoc
        R Commute
        R Collect
        def_true <@ false @> |> Trn |> R  
    ]
 
    /// not not p = p
    let double_negation p = ident prop_calculus <@(not (not %p)) = %p @> [
         LR Collect
         def_false p |> Trn |> LR
    ]

    /// not p = q = p = not q
    let symm_not_eq p q = ident prop_calculus <@ not %p = %q = %p = not %q @> [
         Collect |> L
         RightAssoc |> LR
         Commute |> R
         Collect |> R
         Commute |> R
    ]

    /// p ||| q = q ||| p
    let commute_or p q = id_ax prop_calculus <@ (%p ||| %q) = (%q ||| %p) @>
 
    /// p ||| (q ||| r) = p ||| q ||| r
    let left_assoc_or p q r = ident prop_calculus <@ (%p ||| (%q ||| %r)) = ((%p ||| %q) ||| %r) @> [LR LeftAssoc]

    /// (p ||| q) ||| r = p ||| (q ||| r)
    let right_assoc_or p q r = left_assoc_or p q r  |> Trn

    /// p ||| (q = r) = (p ||| q) = (p ||| r)
    let distrib_or_eq p q r = id_ax prop_calculus <@ (%p ||| (%q = %r)) = ((%p ||| %q) = (%p ||| %r)) @>

    /// (p ||| q) = (p ||| r) = p ||| (q ||| r)
    let collect_or_eq p q r = distrib_or_eq p q r |> Trn

    /// p ||| true = true
    let zero_or p = ident prop_calculus <@ (%p ||| true) = true @> [
        def_true p |> LR 
        LR Distrib
        LR Idemp
    ]

    /// p ||| false = p
    let ident_or p = ident prop_calculus <@ (%p ||| false) = %p @> [
        def_false p |> LR
        L Distrib
        LR RightAssoc
        R Idemp
        def_true p |> R
        L ExcludedMiddle
    ]

    /// (p ||| q) = (p ||| not q = p)
    let ident2_or p q = ident prop_calculus <@ (%p ||| %q) = ((%p ||| not %q) = %p) @> [
        LR LeftAssoc
        collect_or_eq <@ %p @> <@ %q @> <@ not %q @> |> L
        commute_eq <@ %q @> <@ not %q @> |> L
        def_false <@ %q @> |> Trn |> L
        ident_or <@ %p @> |> L      
    ]

    /// (p ||| p) = p
    let idemp_or p =  id_ax prop_calculus <@ (%p ||| %p) = %p @>
    
    let distrib_or p q r = ident  prop_calculus <@ (%p ||| (%q ||| %r)) = ((%p ||| %q) ||| (%p ||| %r)) @> [Distrib |> L]

    let collect_or p q r = distrib_or p q r |> Trn

    /// not (p = q) = not p = q
    let distrib_not p q = ident prop_calculus <@ (not (%p = %q)) = (not %p = %q) @> [LR RightAssoc]

    /// (not p = q) = not (p = q) 
    let collect_not p q = distrib_not p q |> Trn

    /// p <> q = not (p = q)
    let def_not_eq p q = ident prop_calculus <@ (%p <> %q) = (not (%p = %q)) @> [
        RightAssoc |> LR
    ]

    /// p <> q = q <> p
    let commute_not_eq p q = ident prop_calculus <@ (%p <> %q) = (%q <> %p) @> [
        def_not_eq p q |> L
        id_ax prop_calculus <@ (%q <> %p) = not (%q = %p) @> |> R
        R Commute
    ]

    /// (p <> q) <> r = p <> (q <> r)
    let right_assoc_not_eq p q r = ident prop_calculus <@ ((%p <> %q) <> %r) = (%p <> (%q <> %r)) @> [
        def_not_eq <@ %p @> <@ %q @> |> L
        def_not_eq <@ not (%p = %q) @> <@ %r @> |> L
        def_not_eq <@ %q @> <@ %r @> |> R
        def_not_eq <@ %p @> <@ not (%q = %r) @> |> R
        distrib_not <@ %q @> <@ %r @> |> R
        LeftAssoc |> R
        commute_eq <@ %p @> <@ not %q @> |> R
        collect_not <@ %q @> <@ %p @> |> R
        commute_eq <@ %q @> <@ %p @> |> R
    ]

    /// p <> (q <> r) = (p <> q) <> r  
    let left_assoc_not_eq p q r = right_assoc_not_eq p q r |> Trn

    let golden_rule p q = id_ax prop_calculus <@ (%p |&| %q) = (%p = %q = (%p ||| %q)) @>

    /// p |&| q = q |&| p
    let commute_and p q = ident prop_calculus <@ (%p |&| %q) = (%q |&| %p) @> [
        LR GoldenRule
        commute_or <@ %q @> <@ %p @> |> R
        commute_eq <@ %q @> <@ %p @> |> R 
    ]

    /// p |&| (q |&| r) = p |&| q |&| r
    let left_assoc_and p q r = ident prop_calculus <@ (%p |&| (%q |&| %r) = (%p |&| %q |&| %r)) @> [
        LeftAssoc |> LR
    ]

    /// p |&| q |&| r = p |&| (q |&| r)
    let right_assoc_and p q r = ident prop_calculus <@ (%p |&| %q |&| %r) = (%p |&| (%q |&| %r)) @> [
        RightAssoc |> LR
    ]
    
    /// p |&| p = p
    let idemp_and p = ident prop_calculus <@ (%p |&| %p) = %p @> [
        LR GoldenRule
        RightAssoc |> LR
        idemp_or <@ %p @> |> Taut |> R
        LR Commute 
    ] 
    
    /// p |&| true = p
    let ident_and p = proof prop_calculus <@ (%p |&| true) = %p @> [
        LR GoldenRule
        LR RightAssoc
        zero_or <@ %p @> |> R
        R Commute
    ]

    /// p |&| false = false
    let zero_and p = ident prop_calculus <@ (%p |&| false) = false @> [
      LR GoldenRule
      ident_or <@ %p @> |> L
      LR RightAssoc
    ]

    /// p |&| not p = false
    let contr p = ident prop_calculus <@ %p |&| not %p = false@> [
        GoldenRule |> L
        ExcludedMiddle |> L
        commute_eq <@ %p @> <@ not %p @> |> L
        def_false <@ %p @> |> Trn |> L
        commute_eq <@ false @> <@ true @> |> L
        RightAssoc |> LR
    ]

    let absorb_and p q = ident prop_calculus <@ (%p |&| (%p ||| %q)) = %p @> [
        L GoldenRule
        left_assoc_or <@ %p @> <@ %p @> <@ %q @> |> L
        idemp_or <@ %p @> |> L
    ]

    let absorb_or p q = ident prop_calculus <@ (%p ||| (%p |&| %q)) = %p @> [
        GoldenRule |> L
        Distrib |> L
        Distrib |> R
        left_assoc_or <@ %p @> <@ %p @> <@ %q @> |> L
        idemp_or <@ %p @> |> L
        Distrib |> L
        idemp_or <@ %p @> |> L
    ]