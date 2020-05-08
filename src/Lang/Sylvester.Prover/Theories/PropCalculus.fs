namespace Sylvester

open FSharp.Quotations

/// Propositional calculus using the axioms and rules of S.
module PropCalculus =
    let prop_calculus = Theory.S

    (* Expression functions for admissible rules *)
    
    let _def_true = EquationalLogic._def_true

    let _def_false = EquationalLogic._def_false

    let _reduce_constants = EquationalLogic._reduce_constants

    let _left_assoc = EquationalLogic._left_assoc

    let _right_assoc = EquationalLogic._right_assoc

    let _commute = EquationalLogic._commute

    let _distrib = EquationalLogic._distrib

    let _collect = EquationalLogic._collect

    let _idemp = EquationalLogic._idemp

    let _excluded_middle = EquationalLogic._excluded_middle

    let _golden_rule = EquationalLogic._golden_rule

    (* Admissible rules *)

    /// Substitute definition of true in expression.
    let DefTrue = Theory.S.Rules.[0]

    /// Substitute definition of false in expression.
    let DefFalse = Theory.S.Rules.[1]

    /// Reduce logical constants in expression. 
    let Reduce = Theory.S.Rules.[2]

    /// Logical expression is left associative.
    let LeftAssoc = Theory.S.Rules.[3]

    /// Logical expression is right associative.
    let RightAssoc = Theory.S.Rules.[4]
  
    /// Logical expression is commutative.
    let Commute = Theory.S.Rules.[5]

    /// Distribute logical terms in expression.
    let Distrib = Theory.S.Rules.[6]

    /// Collect distributed logical terms in expression.
    let Collect = Theory.S.Rules.[7]

    /// Logical operators are idempotent.
    let Idemp = Theory.S.Rules.[8]

    /// Logical expression satisfies law of excluded middle.
    let ExcludedMiddle = Theory.S.Rules.[9]

    /// Logical expression satisfies golden rule.
    let GoldenRule = Theory.S.Rules.[10]

    let Implication = Theory.S.Rules.[11]

    (* proof step shortcuts *)
    
    let ppc_id steps expr = ident prop_calculus steps expr
    let ppc_id_ax expr = id_ax prop_calculus expr
     
    (* Tactics *)

    /// The constant true is a theorem
    let Truth = Tactics.Truth Commute

    /// If A = B is a theorem then so is B = A.
    let Trn = Tactics.Trn Commute
    
    /// If (L = R) = B is a theorem then so is (R = L) = B.
    let TrnL = Tactics.TrnL Commute

    /// If A = (L = R) is a theorem then so is A = (R = L).
    let TrnR = Tactics.TrnR Commute

    /// If A = B is a theorem then so is (A = B) = true.
    let Taut =  
        let ieq p = Theorem(<@@ ((%%p) = true) = (%%p) @@>, Proof (<@@ (%%p = true) = %%p @@>, prop_calculus, [L Commute; LR RightAssoc], true)) |> Ident  
        Tactics.Taut ieq

    let Lemma = Taut >> Truth >> LR

    let LeftAssoc' = Tactics.LeftAssoc' RightAssoc

    let RightAssoc' = Tactics.RightAssoc' LeftAssoc

    (* Derived rules *)
    
    /// true = (p = p)
    let def_true p = id_ax prop_calculus <@ true = (%p = %p) @>  

    /// false = (not p = p)
    let def_false p = ident prop_calculus <@ false = (not %p = %p) @> [
        R Collect
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
    let right_assoc_eq p q r = id_ax prop_calculus <@ ((%p = %q) = %r) = (%p = (%q = %r)) @>

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
         not_false |> Truth |> LR
    ]

    /// not p = q = p = not q
    let symm_not_eq p q = ident prop_calculus <@ not %p = %q = %p = not %q @> [
        Collect |> L
        RightAssoc |> LR
        Collect |> L
        Commute |> R
        Collect |> R
        commute_eq q p |> R
    ]

    /// (p = q) = (not p = not q)
    let symm_eq_not_eq p q = ident prop_calculus <@ %p = %q = (not %p = not %q) @> [
        LeftAssoc |> LR
        commute_eq <@ %p = %q @> <@ not %p @> |> L
        commute_eq p q |> L
        LeftAssoc |> LR
        symm_not_eq p q |> Lemma
    ]

    /// p ||| q = q ||| p
    let commute_or p q = id_ax prop_calculus <@ (%p ||| %q) = (%q ||| %p) @>
 
    /// p ||| (q ||| r) = p ||| q ||| r
    let left_assoc_or p q r = ident prop_calculus <@ (%p ||| (%q ||| %r)) = ((%p ||| %q) ||| %r) @> [LR LeftAssoc; LR Commute]

    /// (p ||| q) ||| r = p ||| (q ||| r)
    let right_assoc_or p q r = left_assoc_or p q r  |> Trn

    /// ((p ||| q) ||| (r ||| s)) = ((p ||| r) ||| (q ||| s))
    let commute_or_or p q r s = ident prop_calculus <@ ((%p ||| %q) ||| (%r ||| %s)) = ((%p ||| %r) ||| (%q ||| %s)) @> [
        left_assoc_or <@ %p ||| %q@> r s |> L
        right_assoc_or p  q r |> L
        commute_or q r |> L
        left_assoc_or p r q |> L
    ]

    /// p ||| (q = r) = (p ||| q) = (p ||| r)
    let distrib_or_eq p q r = id_ax prop_calculus <@ (%p ||| (%q = %r)) = ((%p ||| %q) = (%p ||| %r)) @>

    /// (p ||| q) = (p ||| r) = p ||| (q ||| r)
    let collect_or_eq p q r = distrib_or_eq p q r |> Trn

    /// (p ||| p) = p
    let idemp_or p =  id_ax prop_calculus <@ (%p ||| %p) = %p @>

    /// p ||| true = true
    let zero_or p = ident prop_calculus <@ (%p ||| true) = true @> [
        def_true p |> LR 
        Distrib |> L
        Distrib |> R
        idemp_or p |> L
    ]

    /// p ||| false = p
    let ident_or p = ident prop_calculus <@ (%p ||| false) = %p @> [
        def_false p |> L
        L Distrib
        LR RightAssoc
        idemp_or p |> R
        L ExcludedMiddle
    ]

    /// (p ||| q) = (p ||| not q = p)
    let ident_or_or_not p q = ident prop_calculus <@ (%p ||| %q) = ((%p ||| not %q) = %p) @> [
        LR LeftAssoc
        collect_or_eq p q <@ not %q @> |> L
        commute_eq  q  <@ not %q @> |> L
        def_false q  |> Trn |> L
        ident_or p  |> L      
    ]

    /// (p ||| not q) = (p = (p or q))
    let ident_or_not_or p q = ident prop_calculus <@ (%p ||| not %q) = (%p = (%p ||| %q)) @> [
        Commute |> R
        LR LeftAssoc
        collect_or_eq p  <@ not %q @> q  |> L
        def_false q |> Trn |> L
        ident_or p |> L
    ]

    
    /// p ||| (q ||| r) = ((p ||| q) ||| (p ||| r))
    let distrib_or p q r = ident prop_calculus <@ (%p ||| (%q ||| %r)) = ((%p ||| %q) ||| (%p ||| %r)) @> [
        LeftAssoc |> R
        right_assoc_or p  q  p |> R
        commute_or q p |> R
        LeftAssoc |> R
        commute_or p q |> R
        idemp_or p |> R
        LeftAssoc |> L
     ]

    /// (p ||| q) = (p ||| r) = p ||| (q ||| r)
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
        def_not_eq <@ p @> <@ q @> |> L
        def_not_eq <@ q @> <@ p @> |> R
        commute_eq <@ q @> <@ p @> |> R
    ]

    /// (p <> q) <> r = p <> (q <> r)
    let right_assoc_not_eq p q r = ident prop_calculus <@ ((%p <> %q) <> %r) = (%p <> (%q <> %r)) @> [
        def_not_eq p q  |> L
        def_not_eq <@ not (%p = %q) @> r |> L
        def_not_eq q r |> R
        def_not_eq p <@ not (%q = %r) @> |> R
        distrib_not q r |> R
        left_assoc_eq p <@ not %q @> r |> R
        commute_eq p <@ not %q @> |> R
        collect_not q p |> R
        commute_eq q  p |> R
    ]

    /// p <> (q <> r) = (p <> q) <> r  
    let left_assoc_not_eq p q r = right_assoc_not_eq p q r |> Trn

    /// p |&| q = ((p = q) = (p ||| q))
    let golden_rule p q = id_ax prop_calculus <@ (%p |&| %q) = (%p = %q = (%p ||| %q)) @>

    /// p |&| q = q |&| p
    let commute_and p q = ident prop_calculus <@ (%p |&| %q) = (%q |&| %p) @> [
        golden_rule <@ %p @> <@ %q @> |> L
        golden_rule <@ %q @> <@ %p @> |> R
        commute_or q p |> R
        commute_eq q p |> R 
    ]

    let ident_and_eq_all p q r = ident prop_calculus <@ (%p |&| %q |&| %r) = (%p = %q = %r = (%p ||| %q) = (%q ||| %r) = (%r ||| %p) = (%p ||| %q ||| %r)) @> [
        golden_rule p q |> L
        golden_rule <@ (%p = %q) = (%p ||| %q) @> r |> L 
        commute_or <@ ((%p = %q) = (%p ||| %q)) @> r |> L
        distrib_or_eq r <@ %p = %q @> <@ %p ||| %q @> |> L
        distrib_or_eq r p q |> L
        right_assoc_eq <@ %p = %q@> <@ %p ||| %q @> r |> L
        commute_eq <@ %p ||| %q @> r |> L
        commute_or r q |> L
        commute_eq <@ %r ||| %p @> <@ %q ||| %r @> |> L
        commute_or r <@ %p ||| %q @> |> L
        L LeftAssoc
        LeftAssoc |> L |> L'
        LeftAssoc |> L |> L' |> L' |> L'
    ]
    
    /// p |&| q |&| r = p |&| (q |&| r)
    let right_assoc_and p q r = ident prop_calculus <@ (%p |&| %q |&| %r) = %p |&| (%q |&| %r) @> [
        golden_rule p q |> L
        golden_rule <@ (p = q) = (%p ||| %q) @> r |> L 
        commute_or <@ ((%p = %q) = (%p ||| %q)) @> r |> L
        distrib_or_eq r <@ %p = %q @> <@ %p ||| %q @> |> L
        distrib_or_eq r p q |> L
        right_assoc_eq <@ %p = %q@> <@ %p ||| %q@> r |> L
        commute_eq <@ %p ||| %q @> r |> L
        commute_or r q |> L
        commute_eq <@ %r ||| %p @> <@ %q ||| %r @> |> L
        commute_or r <@ %p ||| %q @> |> L
        L LeftAssoc
        L LeftAssoc
        L LeftAssoc
        commute_and p <@ %q |&| %r @> |> R
        ident_and_eq_all q r p |> R
        commute_eq <@ %q = %r @> p |> R
        left_assoc_eq p q r |> R
        commute_or <@ %q ||| %r @> p |> R
        left_assoc_or p q r |> R
        right_assoc_eq <@ %p = %q = %r @> <@ %q ||| %r @> <@ %r ||| %p @> |> R
        RightAssoc |> L |> R'
        Commute |> R |> L' |> R'
        LeftAssoc |> R |> L' |> R'
        LeftAssoc |> L |> R'
        LeftAssoc |> L |> L' |> R'
    ]

    /// p |&| (q |&| r) = p |&| q |&| r
    let left_assoc_and p q r = right_assoc_and p q r |> Trn

    /// p |&| p = p
    let idemp_and p = ident prop_calculus <@ (%p |&| %p) = %p @> [
        LR GoldenRule
        RightAssoc |> LR
        idemp_or p |> Taut |> R
        LR Commute 
    ] 
    
    /// p |&| true = p
    let ident_and p = proof prop_calculus <@ (%p |&| true) = %p @> [
        LR GoldenRule
        LR RightAssoc
        zero_or p |> R
        R Commute
    ]

    /// p |&| false = false
    let zero_and p = ident prop_calculus <@ (%p |&| false) = false @> [
      golden_rule p <@ false @> |> L
      ident_or p |> L
      LR RightAssoc
    ]

    /// p |&| (q |&| r) = (p |&| q) |&| (p |&| r)
    let distrib_and p q r = ident prop_calculus <@ (%p |&| (%q |&| %r)) = ((%p |&| %q) |&| (%p |&| %r)) @> [
        LeftAssoc |> R
        right_assoc_and p  q  p |> R
        commute_and q p |> R
        LeftAssoc |> R
        idemp_and p |> R
        LeftAssoc |> L
    ]
    /// p |&| not p = false
    let contr p = ident prop_calculus <@ %p |&| not %p = false@> [
        GoldenRule |> L
        ExcludedMiddle |> L
        commute_eq p  <@ not %p @> |> L
        def_false p  |> Trn |> L
        commute_eq <@ false @> <@ true @> |> L
        RightAssoc |> LR
    ]

    /// (p |&| (p ||| q)) = p
    let absorb_and p q = ident prop_calculus <@ (%p |&| (%p ||| %q)) = %p @> [
        L GoldenRule
        left_assoc_or p  p  q |> L
        idemp_or p |> L
    ]

    /// (p ||| (p |&| q)) = p
    let absorb_or p q = ident prop_calculus <@ (%p ||| (%p |&| %q)) = %p @> [
        GoldenRule |> L
        Distrib |> L
        Distrib |> R
        left_assoc_or p p q  |> L
        idemp_or p |> L
        Distrib |> L
        idemp_or p  |> L
    ]

    /// p |&| (not p ||| q) = (p |&| q)
    let absorb_and_not p q = proof prop_calculus <@ %p |&| (not %p ||| %q) = (%p |&| %q) @> [
        GoldenRule |> L
        left_assoc_or p <@ not %p @> q |> L
        ExcludedMiddle |> L
        zero_or q |> TrnL |> L
        ident_eq <@ %p = (not %p ||| %q) @> |> L
        commute_or <@ not %p @> q |> L
        ident_or_not_or q p |> L
        LeftAssoc |> L
        commute_or q p |> L
        golden_rule p q |> Trn |> L
    ]

    /// p ||| (not p |&| q) = (p ||| q)
    let absorb_or_not p q = ident prop_calculus <@ %p ||| (not %p |&| %q) = (%p ||| %q) @> [
        GoldenRule |> L
        commute_or <@ not %p @> q  |> L
        right_assoc_eq <@ not %p @> q  <@ %q ||| not %p @> |> L
        ident_or_or_not q  p |> Trn |> TrnL |> L
        Distrib |> L 
        commute_or q p |> L
        left_assoc_or p p q |> L
        idemp_or p |> L
        ExcludedMiddle |> L
        ident_eq <@ %p ||| %q @> |> TrnL |> L
    ]
    
    /// p ||| (q |&| r) = ((p ||| q) |&| (p ||| r))
    let distrib_or_and p q r = ident prop_calculus <@ %p ||| (%q |&| %r) = ((%p ||| %q) |&| (%p ||| %r)) @> [
        GoldenRule |> L
        GoldenRule |> R
        Distrib |> L
        Distrib |> L
        distrib_or p q r |> L
    ]

    /// not (p |&| q) = not p ||| not q
    let distrib_not_and p q = ident prop_calculus <@ not (%p |&| %q) = (not %p ||| not %q) @> [
        GoldenRule |> L
        Distrib |> L
        Distrib |> L 
        ident_or_or_not <@ not %p @> <@ not %q @> |> R
        double_negation q |> R
        ident_or_not_or q p |> TrnL |> R
        Commute |> R
        commute_or q p |> R
    ]

    /// not (p ||| q) = not p |&| not q
    let distrib_not_or p q = ident prop_calculus <@ not (%p ||| %q) = (not %p |&| not %q) @> [
        golden_rule p q |> Trn |> TrnL |> RightAssoc' |> L
        Commute |> L
        Distrib |> L
        distrib_not_and p q |> L
        Commute |> LR
        symm_eq_not_eq p q |> R
        Commute |> R
    ]