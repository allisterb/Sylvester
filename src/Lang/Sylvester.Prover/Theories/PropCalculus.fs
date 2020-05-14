namespace Sylvester

open FSharp.Quotations

/// Propositional calculus using the axioms and rules of S.
module PropCalculus =
    let prop_calculus = Theory.S

    (* Expression functions for admissible rules *)
    
    let _reduce_constants = EquationalLogic._reduce_constants

    let _left_assoc = EquationalLogic._left_assoc

    let _right_assoc = EquationalLogic._right_assoc

    let _commute = EquationalLogic._commute

    let _distrib = EquationalLogic._distrib

    let _collect = EquationalLogic._collect

    let _idemp = EquationalLogic._idemp

    let _excluded_middle = EquationalLogic._excluded_middle

    let _golden_rule = EquationalLogic._golden_rule

    let _shunt = EquationalLogic._shunt

    let _distrib_implies = EquationalLogic._distrib_implies

    (* Admissible rules *)

    /// Reduce logical constants in expression. 
    let reduce = Theory.S.Rules.[0]

    /// Logical expression is left associative.
    let left_assoc = Theory.S.Rules.[1]

    /// Logical expression is right associative.
    let right_assoc = Theory.S.Rules.[2]
  
    /// Logical expression is commutative.
    let commute = Theory.S.Rules.[3]

    /// Distribute logical terms in expression.
    let distrib = Theory.S.Rules.[4]

    /// Collect distributed logical terms in expression.
    let collect = Theory.S.Rules.[5]

    /// Logical operators are idempotent.
    let idemp = Theory.S.Rules.[6]

    /// Logical expression satisfies law of excluded middle.
    let excluded_middle' = Theory.S.Rules.[7]

    /// Logical expression satisfies golden rule.
    let golden_rule' = Theory.S.Rules.[8]

    let def_implies' = Theory.S.Rules.[9]

    let shunting' = Theory.S.Rules.[10]

    let distrib_implies = Theory.S.Rules.[11]

    (* proof step shortcuts *)
    
    let ppc_id steps expr = ident prop_calculus steps expr
    let ppc_id_ax expr = id_ax prop_calculus expr
     
    (* Tactics *)

    /// The constant true is a theorem
    let Truth = Tactics.Truth commute

    /// If A = B is a theorem then so is B = A.
    let Commute = Tactics.Commute commute
    
    /// If (L = R) = B is a theorem then so is (R = L) = B.
    let CommuteL = Tactics.CommuteL commute

    /// If A = (L = R) is a theorem then so is A = (R = L).
    let CommuteR = Tactics.CommuteR commute

    /// If A = B is a theorem then so is (A = B) = true.
    let Taut =  
        let ieq p = Theorem(<@@ ((%%p) = true) = (%%p) @@>, Proof (<@@ (%%p = true) = %%p @@>, prop_calculus, [L commute; LR right_assoc], true)) |> Ident  
        Tactics.Taut ieq

    /// If A is a theorem then so is A = true.
    let Taut' t = 
        let ieq p = Theorem(<@@ ((%%p) = true) = (%%p) @@>, Proof (<@@ (%%p = true) = %%p @@>, prop_calculus, [L commute; LR right_assoc], true)) |> Ident 
        Tactics.Taut' ieq t
        
    let Lemma = Taut >> Truth >> LR
    
    let Lemma' = Taut' >> Truth >> LR

    let LeftAssoc = Tactics.LeftAssoc right_assoc

    let LeftAssocL = Tactics.LeftAssocL right_assoc

    let LeftAssocR = Tactics.LeftAssocR right_assoc

    let RightAssoc = Tactics.RightAssoc left_assoc

    let RightAssocL = Tactics.RightAssocL left_assoc

    let RightAssocR = Tactics.RightAssocR left_assoc

    (* Derived rules *)
    
    /// true = (p = p)
    let def_true p = id_ax prop_calculus <@ true = (%p = %p) @> 
        
    /// false = (not p = p)
    let def_false p = ident prop_calculus <@ false = (not %p = %p) @> [
        R collect
        def_true p |> Commute |> R
    ] 

    /// (p = true) = p
    let ident_eq p = ident prop_calculus <@ (%p = true) = %p @> [
        L commute
        LR right_assoc
    ]

    /// p = q = q = p
    let commute_eq p q = ident prop_calculus <@ (%p = %q) = (%q = %p) @> [LR left_assoc]

    /// p = (q = r) = p = q = r
    let left_assoc_eq p q r = ident prop_calculus <@ (%p = (%q = %r)) = ((%p = %q) = %r) @> [R right_assoc]

    /// (p = q) = r = p = (q = r)
    let right_assoc_eq p q r = id_ax prop_calculus <@ ((%p = %q) = %r) = (%p = (%q = %r)) @>

    /// not false = true
    let not_false = ident prop_calculus <@not false = true@> [
        LR commute
        def_true <@ false @> |> L
        LR right_assoc
        R commute
        R collect
        def_true <@ false @> |> Commute |> R  
    ]

    /// not not p = p
    let double_negation p = ident prop_calculus <@(not (not %p)) = %p @> [
         LR collect
         def_false p |> Commute |> LR
         not_false |> Truth |> LR
    ]

    /// not p = q = p = not q
    let symm_not_eq p q = ident prop_calculus <@ not %p = %q = %p = not %q @> [
        collect |> L
        right_assoc |> LR
        collect |> L
        commute |> R
        collect |> R
        commute_eq q p |> R
    ]

    /// (p = q) = (not p = not q)
    let symm_eq_not_eq p q = ident prop_calculus <@ %p = %q = (not %p = not %q) @> [
        left_assoc |> LR
        commute_eq <@ %p = %q @> <@ not %p @> |> L
        commute_eq p q |> L
        left_assoc |> L
        symm_not_eq p q |> Lemma
    ]

    /// ((p = q) = (r = s)) = ((p = r) = (q = s))
    let commute_eq_eq p q r s = ident prop_calculus <@ ((%p = %q) = (%r = %s)) = ((%p = %r) = (%q = %s)) @> [
        left_assoc_eq <@ %p = %q@> r s |> L
        right_assoc_eq p q r |> L
        commute_eq q r |> L
        left_assoc_eq p r q |> L
    ]

    /// p ||| q = q ||| p
    let commute_or p q = id_ax prop_calculus <@ (%p ||| %q) = (%q ||| %p) @>
 
    /// p ||| (q ||| r) = p ||| q ||| r
    let left_assoc_or p q r = ident prop_calculus <@ (%p ||| (%q ||| %r)) = ((%p ||| %q) ||| %r) @> [LR left_assoc; LR commute]

    /// (p ||| q) ||| r = p ||| (q ||| r)
    let right_assoc_or p q r = left_assoc_or p q r  |> Commute

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
    let collect_or_eq p q r = distrib_or_eq p q r |> Commute

    /// (p ||| p) = p
    let idemp_or p =  id_ax prop_calculus <@ (%p ||| %p) = %p @>

    /// p ||| true = true
    let zero_or p = ident prop_calculus <@ (%p ||| true) = true @> [
        def_true p |> LR 
        distrib |> L
        distrib |> R
        idemp_or p |> L
    ]

    /// p ||| false = p
    let ident_or p = ident prop_calculus <@ (%p ||| false) = %p @> [
        def_false p |> L
        L distrib
        LR right_assoc
        idemp_or p |> R
        L excluded_middle'
    ]

    /// (p ||| q) = (p ||| not q = p)
    let ident_or_or_not p q = ident prop_calculus <@ (%p ||| %q) = ((%p ||| not %q) = %p) @> [
        LR left_assoc
        collect_or_eq p q <@ not %q @> |> L
        commute_eq  q  <@ not %q @> |> L
        def_false q  |> Commute |> L
        ident_or p  |> L      
    ]

    /// (p ||| not q) = (p = (p or q))
    let ident_or_not_or p q = ident prop_calculus <@ (%p ||| not %q) = (%p = (%p ||| %q)) @> [
        commute |> R
        LR left_assoc
        collect_or_eq p <@ not %q @> q  |> L
        def_false q |> Commute |> L
        ident_or p |> L
    ]

    
    /// p ||| (q ||| r) = ((p ||| q) ||| (p ||| r))
    let distrib_or_or p q r =  ident prop_calculus <@ (%p ||| (%q ||| %r)) = ((%p ||| %q) ||| (%p ||| %r)) @> [
        idemp_or p |> Commute |> L
        right_assoc |> L
        left_assoc_or p q r |> L
        commute_or p q |> L
        right_assoc_or q p r |> L
        left_assoc |> L
    ]

    /// (p ||| q) = (p ||| r) = p ||| (q ||| r)
    let collect_or_or p q r = distrib_or_or p q r |> Commute

    /// not (p = q) = not p = q
    let distrib_not p q = ident prop_calculus <@ (not (%p = %q)) = (not %p = %q) @> [LR right_assoc]

    /// (not p = q) = not (p = q) 
    let collect_not p q = distrib_not p q |> Commute

    /// p <> q = not (p = q)
    let def_not_eq p q = ident prop_calculus <@ (%p <> %q) = (not (%p = %q)) @> [
        right_assoc |> LR
    ]

    /// p <> q = q <> p
    let commute_not_eq p q = ident prop_calculus <@ (%p <> %q) = (%q <> %p) @> [
        def_not_eq p q |> L
        def_not_eq q p |> R
        commute_eq q p |> R
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
    let left_assoc_not_eq p q r = right_assoc_not_eq p q r |> Commute

    /// p ||| not p = true
    let excluded_middle p = ident prop_calculus <@ (%p ||| (not %p)) = true @> [ident_eq <@ (%p ||| (not %p)) @> |> LR]
    
    /// p |&| q = ((p = q) = (p ||| q))
    let golden_rule p q = id_ax prop_calculus <@ (%p |&| %q) = (%p = %q = (%p ||| %q)) @>

    /// p |&| q = q |&| p
    let commute_and p q = ident prop_calculus <@ (%p |&| %q) = (%q |&| %p) @> [
        golden_rule p q |> L
        golden_rule q p |> R
        commute_or q p |> R
        commute_eq q p |> R 
    ]

    let ident_and_eq_all p q r = ident prop_calculus <@ (%p |&| %q |&| %r) = (%p = %q = %r = (%p ||| %q) = (%q ||| %r) = (%r ||| %p) = (%p ||| %q ||| %r)) @> [
        golden_rule p q |> L
        golden_rule <@ (%p = %q) = (%p ||| %q) @> r |> L 
        commute_or <@ ((%p = %q) = (%p ||| %q)) @> r |> L
        distrib_or_eq r <@ %p = %q @> <@ %p ||| %q @> |> L
        distrib_or_eq r p q |> L
        right_assoc_eq <@ %p = %q @> <@ %p ||| %q @> r |> L
        commute_eq <@ %p ||| %q @> r |> L
        commute_or r q |> L
        commute_eq <@ %r ||| %p @> <@ %q ||| %r @> |> L
        commute_or r <@ %p ||| %q @> |> L
        left_assoc_eq <@ %p = %q @> r <@ %p ||| %q @> |> L
        left_assoc |> L
        left_assoc_eq <@ %p = %q = %r = (%p ||| %q) @> <@ %q ||| %r @> <@ %r ||| %p @> |> L
    ]
    
    /// p |&| q |&| r = p |&| (q |&| r)
    let right_assoc_and p q r = ident prop_calculus <@ (%p |&| %q |&| %r) = (%p |&| (%q |&| %r)) @> [
        ident_and_eq_all p q r |> L
        commute_and p <@ %q |&| %r @> |> R
        ident_and_eq_all q r p |> R
        commute_eq <@ %q = %r @> p |> R
        left_assoc_eq <@ %p = %q = %r = (%p ||| %q) @> <@ %q ||| %r @> <@ %r ||| %p @> |> L
        left_assoc_eq p q r |> R
        commute_or <@ %q ||| %r @> p |> R
        left_assoc_or p q r |> R
        right_assoc_eq <@ %p = %q = %r @> <@ %q ||| %r @> <@ %r ||| %p @> |> R
        left_assoc_eq <@ %p = %q = %r @>  <@ %q ||| %r @> <@ %r ||| %p @> |> R
        right_assoc_eq <@ %p = %q = %r = (%q ||| %r) @> <@ %r ||| %p @> <@ %p ||| %q @> |> R
        commute_eq <@ (%r ||| %p) @> <@ %p ||| %q @> |> R
        left_assoc |> R
        left_assoc_eq <@ %p = %q = %r = (%q ||| %r) @> <@ %p ||| %q  @> <@ %r ||| %p @> |> R
        right_assoc_eq <@ %p = %q = %r @>  <@ %q ||| %r @> <@ %p ||| %q @> |> R
        commute_eq <@ %q ||| %r @> <@ %p ||| %q @> |> R
        left_assoc_eq <@ %p = %q = %r @> <@ %p ||| %q @>  <@ (%q ||| %r) @> |> R
    ]

    /// p |&| (q |&| r) = p |&| q |&| r
    let left_assoc_and p q r = right_assoc_and p q r |> Commute

    /// p |&| p = p
    let idemp_and p = ident prop_calculus <@ (%p |&| %p) = %p @> [
        L golden_rule'
        right_assoc |> LR
        idemp_or p |> Taut |> R
        LR commute 
    ] 
    
    /// p |&| true = p
    let ident_and p = proof prop_calculus <@ (%p |&| true) = %p @> [
        L golden_rule'
        LR right_assoc
        zero_or p |> R
        R commute
    ]

    /// p |&| false = false
    let zero_and p = ident prop_calculus <@ (%p |&| false) = false @> [
      golden_rule p <@ false @> |> L
      ident_or p |> L
      LR right_assoc
    ]

    /// p |&| (q |&| r) = (p |&| q) |&| (p |&| r)
    let distrib_and p q r = ident prop_calculus <@ (%p |&| (%q |&| %r)) = ((%p |&| %q) |&| (%p |&| %r)) @> [
        idemp_and p |> Commute |> L
        right_assoc |> L
        left_assoc_and p q r |> L
        commute_and p q |> L
        right_assoc_and q p r |> L
        left_assoc |> L
    ]

    /// p |&| not p = false
    let contr p = ident prop_calculus <@ %p |&| not %p = false@> [
        golden_rule' |> L
        excluded_middle' |> R |> L'
        commute_eq p <@ not %p @> |> L
        def_false p |> Commute |> L
        commute_eq <@ false @> <@ true @> |> L
        right_assoc |> LR
    ]

    /// (p |&| (p ||| q)) = p
    let absorb_and p q = ident prop_calculus <@ (%p |&| (%p ||| %q)) = %p @> [
        L golden_rule'
        left_assoc_or p  p  q |> L
        idemp_or p |> L
    ]

    /// (p ||| (p |&| q)) = p
    let absorb_or p q = ident prop_calculus <@ (%p ||| (%p |&| %q)) = %p @> [
        golden_rule' |> R |> L'
        distrib |> L
        left_assoc_or p p q |> L
        idemp_or p |> L
        distrib_or_eq p p q |> L
        idemp_or p |> L
    ]

    /// p |&| (not p ||| q) = (p |&| q)
    let absorb_and_not p q = ident prop_calculus <@ %p |&| (not %p ||| %q) = (%p |&| %q) @> [
        golden_rule' |> L
        left_assoc_or p <@ not %p @> q |> L
        excluded_middle p |> L
        zero_or q |> CommuteL |> L
        ident_eq <@ %p = (not %p ||| %q) @> |> L
        commute_or <@ not %p @> q |> L
        ident_or_not_or q p |> L
        left_assoc |> L
        commute_or q p |> L
        golden_rule p q |> Commute |> L
    ]

    /// p ||| (not p |&| q) = (p ||| q)
    let absorb_or_not p q = ident prop_calculus <@ %p ||| (not %p |&| %q) = (%p ||| %q) @> [
        golden_rule' |> L
        commute_or <@ not %p @> q  |> L
        right_assoc_eq <@ not %p @> q  <@ %q ||| not %p @> |> L
        ident_or_or_not q  p |> Commute |> CommuteL |> L
        distrib |> L 
        commute_or q p |> L
        left_assoc_or p p q |> L
        idemp_or p |> L
        excluded_middle' |> L
        ident_eq <@ %p ||| %q @> |> CommuteL |> L
    ]
    
    /// p ||| (q |&| r) = ((p ||| q) |&| (p ||| r))
    let distrib_or_and p q r = ident prop_calculus <@ %p ||| (%q |&| %r) = ((%p ||| %q) |&| (%p ||| %r)) @> [
        golden_rule' |> R |> L'
        distrib |> L
        distrib |> L |> L'
        distrib_or_or p q r |> L
        golden_rule <@ %p ||| %q @> <@ %p ||| %r @> |> Commute |> L
    ]

    let collect_or_and p q r = distrib_or_and p q r |> Commute

    /// p |&| (q ||| r) = ((p |&| q) ||| (p |&| r))
    let distrib_and_or p q r =  ident prop_calculus <@ %p |&| (%q ||| %r) = ((%p |&| %q) ||| (%p |&| %r)) @> [
        distrib_or_and <@ %p |&| %q @> p r|> R
        absorb_or p q |> CommuteL |> R
        distrib_or_and r p q |> CommuteL |> R
        left_assoc |> R
        commute_or r p |>R
        absorb_and p r |> R
        commute |> R |> R'
    ]
    /// not (p |&| q) = not p ||| not q
    let distrib_not_and p q = ident prop_calculus <@ not (%p |&| %q) = (not %p ||| not %q) @> [
        golden_rule' |> LR |> LR' |> L'
        distrib |> L
        distrib |> L |> L' 
        ident_or_or_not <@ not %p @> <@ not %q @> |> R
        double_negation q |> R
        ident_or_not_or q p |> CommuteL |> R
        commute |> R
        commute_or q p |> R
    ]

    /// not (p ||| q) = not p |&| not q
    let distrib_not_or p q = ident prop_calculus <@ not (%p ||| %q) = (not %p |&| not %q) @> [
        golden_rule p q |> Commute |> CommuteL |> RightAssoc |> L
        commute |> LR |> LR' |> L'
        distrib |> L
        distrib_not_and p q |> L
        commute |> LR
        symm_eq_not_eq p q |> R
        commute |> R
    ]

    let ident_or_or_not_eq p q = ident prop_calculus <@ (%p ||| %q) = (%p ||| not %q = %p) @> [
        left_assoc |> LR
        collect_or_eq p q <@ not %q @> |> LR
        commute_eq q <@ not %q @> |> L
        def_false q |> Commute |> L
        ident_or p |> L
    ]

    let ident_eq_and_or_and p q = ident prop_calculus <@ (%p = %q) = (%p |&| %q) ||| (not %p |&| not %q) @> [
        collect |> R
        commute |> L |> L'
        commute |> L
        commute |> R |> L'
        golden_rule p q |> LeftAssoc |> L
    ]
    let ident_and_and_not p q = ident prop_calculus <@ (%p |&| %q) = (%p |&| not %q = not %p) @> [
        left_assoc |> LR
        golden_rule' |> L |> L'
        golden_rule p <@ not %q @> |> L
        commute |> R |> L'
        left_assoc |> R |> L'
        ident_or_or_not_eq p q |> Commute |> L
        left_assoc |> L
        right_assoc |> L |> L'
        def_true <@ %p ||| %q @> |> Commute |> L
        commute |> L |> L'
        right_assoc |> LR
        commute |> R
        right_assoc |> LR
        symm_eq_not_eq p q |> R
    ]

    let distrib_and_eq p q r = ident prop_calculus <@ %p |&| (%q = %r) =   ((%p |&| %q) = (%p |&| %r) = %p) @> [
        golden_rule' |> L
        distrib |> R |> L'
        left_assoc |> L |> L'
        left_assoc |> L
        right_assoc |> L
        commute_eq_eq <@ %p = %q @> r <@ %p ||| %q @> <@ %p ||| %r @> |> L
        golden_rule p q |> LeftAssoc |> L
        golden_rule p r |> LeftAssoc |> LeftAssocL |> RightAssoc |> Commute |> L 
        golden_rule p q |> Commute |> L
        left_assoc |> L
    ]

    let ident_and_eq p q  = ident prop_calculus <@ %p |&| (%q = %p) = (%p |&| %q) @> [
        golden_rule' |> L
        distrib |> R |> L'
        left_assoc |> L |> L'
        left_assoc |> L
        right_assoc |> L
        idemp_or p |> L
        commute |> L |> L'
        left_assoc |> L |> L'
        def_true p |> Commute |> L |> L'
        ident_eq q |> CommuteL |> L
        commute |> L
        golden_rule p q |> Commute |> CommuteL |> LeftAssocL |> L
    ]

    /// p ==> q = (not p ||| q)
    let ident_implies_not_or p q = ident prop_calculus <@ %p ==> %q = (not %p ||| %q) @> [
        def_implies' |> L
        ident_or_not_or q p |> CommuteL |> R
        commute |> R
        commute |> L |> R'
    ]

    /// p ==> q = ((p |&| q) = p)
    let ident_implies_eq_and_eq p q = ident prop_calculus <@ %p ==> %q = ((%p |&| %q) = %p) @> [
        def_implies' |> L
        commute |> LR
        right_assoc |> LR
        commute |> R |> R' 
        left_assoc |> R 
    ]

    /// p ==> q = (not q ==> not p)
    let def_implies_contr p q = ident prop_calculus <@ %p ==> %q = (not %q ==> not %p) @> [
        def_implies' |> R
        commute |> R
        commute |> R |> R'
        distrib_not_and p q |> Commute |> R |> R'
        symm_eq_not_eq p <@ %p |&| %q @> |> Commute |> R 
        commute |> R
        ident_implies_eq_and_eq p q |> Lemma
    ]

    // p ==> (q = r) = ((p |&| q) = (p |&| r))
    let distrib_implies_eq_and p q r =
        ident prop_calculus <@ %p ==> (%q = %r) = ((%p |&| %q) = (%p |&| %r))@> [
            ident_implies_eq_and_eq p <@ %q = %r @> |> L
            distrib_and_eq p q r |> L
        ]

    /// p ==> (q = r) = ((p ==> q) = (p ==> r))
    let distrib_implies_eq_implies p q r = ident prop_calculus <@ %p ==> (%q = %r) = ((%p ==> %q) = (%p ==> %r))@> [
        distrib_implies_eq_and p q r |> L
        ident_implies_eq_and_eq p q |> L |> R'
        ident_implies_eq_and_eq p r |> R |> R'
        commute |> R |> R'
        left_assoc |> R 
        right_assoc |> L |> R'
        def_true p |> Commute |> R
        ident_eq <@ %p |&| %q @> |> L |> R'
    ]

    /// p |&| (p ==> q) = (p |&| q)
    let ident_and_implies p q = ident prop_calculus <@ %p |&| (%p ==> %q) = (%p |&| %q) @> [
        ident_implies_eq_and_eq p q |> L
        distrib_and_eq p <@ %p |&| %q @> p |> L
        left_assoc |> L |> L' |> L'
        idemp_and p |> L
    ]

    let ident_or_implies p q = ident prop_calculus <@ (%p ||| (%p ==> %q)) = true @> [
        def_implies' |> R |> L'
        distrib |> L
        left_assoc |> L |> L'
        idemp_or p |> L
        ident_eq <@ (%p ||| %q) = (%p ||| %q) @> |> LR
    ]

    let ident_or_conseq p q = ident prop_calculus <@ %p ||| (%q ==> %p) = (%q ==> %p) @> [
        def_implies' |> R |> L'
        distrib |> L
        commute_or q p |> L
        left_assoc_or p p q |> L 
        idemp_or p |> L
        commute |> LR
        commute_or p q |> R
    ]

    let reflex_implies p = theorem prop_calculus <@ p ==> p @> [
        def_implies' |> LR
    ]
        
    let implies_true p = theorem prop_calculus <@ %p ==> true @> [
        def_implies' |> LR
        zero_or p |> L
    ]

    let conseq_false p = theorem prop_calculus <@ false ==> %p @> [
        def_implies' |> LR
        ident_or p |> CommuteL |> Lemma
    ]

    let ident_conseq_true p = ident prop_calculus <@ (true ==> %p) = %p @> [
        def_implies' |> L
        zero_or p |> CommuteL |> L
        right_assoc |> LR
        commute |> LR
    ]

    let ident_implies_false_not p = ident prop_calculus <@ (%p ==> false) = not %p @> [
        def_implies' |> L
        ident_or p |> L
        commute |> LR
        left_assoc |> LR
        commute |> LR
        def_false p |> LR
    ]
    

    let weaken p q = theorem prop_calculus <@ %p ==> (%p ||| %q) @> [
        ident_eq <@ (%p ==> (%p ||| %q)) @> |> LR
        def_implies' |> LR
        left_assoc |> L
        idemp_or p |> L
    ]

    let weaken_and p q = theorem prop_calculus <@ (%p |&| %q ) ==> %p @> [
        ident_eq <@ ((%p |&| %q ) ==> %p) @> |> LR
        def_implies' |> LR
        commute |> L
        absorb_or p q |> Lemma
    ]
    let weaken_or p q = theorem prop_calculus <@ %p ==> (%p ||| %q) @> [
        ident_eq <@ (%p ==> (%p ||| %q)) @> |> LR
        def_implies' |> LR
        left_assoc |> L
        idemp_or p |> L
    ]

    let weaken_and_or p q = theorem prop_calculus <@ %p |&| %q ==> %p ||| %q @> [
        def_implies' |> L
        commute |> L |> L'
        distrib |> L |> L'
        commute |> LR
        idemp_or p |> LR
        distrib |> L |> R'
        idemp_and p |> LR
        distrib |> LR
        distrib |> R |> L'
        distrib |> L
        idemp_or p |> L
        distrib |> L
        commute_or q p |> L
        idemp_and <@ %p ||| %q @> |> L
        commute |> L |> L'
        distrib |> L |> L'
        idemp_and q |> L
        absorb_or q p |> CommuteL |> L
        commute |> R |> L'
        left_assoc |> L
        idemp_or q |> L
    ]

    let weaken_or_and p q r= theorem prop_calculus <@ (%p ||| (%q |&| %r)) ==> (%p ||| %q) @> [
        distrib |> L
        weaken_and <@ %p ||| %q @> <@ %p ||| %r @> |> Lemma'
    ]

    let weaken_and_and_or p q r= theorem prop_calculus <@ (%p |&| %q)  ==> (%p |&| (%q ||| %r)) @> [
        distrib |> R
        weaken <@ %p |&| %q @> <@ %p |&| %r @> |> Lemma'
    ]

    let modus_ponens p q = theorem prop_calculus <@ %p |&| (%p ==> %q) ==> %q @> [
        ident_and_implies p q |> L
        commute_and p q |> LR
        weaken_and q p |> Lemma'
    ]

    let case_analysis_1 p q r = ident prop_calculus <@( %p ==> %r) |&| (%q ==> %r) = (%p ||| %q  ==> %r) @> [
        ident_implies_not_or <@ %p ||| %q @> r |> R
        distrib|> L |> R'
        distrib_or_and r <@ not %p @> <@ not %q @> |> CommuteL |> R
        commute |> L |> R'
        commute |> R |> R'
        ident_implies_not_or p r |> Commute |> R
        ident_implies_not_or q r |> Commute |> R
    ]

    let case_analysis_2 p r = ident prop_calculus <@ (%p ==> %r) |&| (not %p ==> %r ) = %r @> [
        case_analysis_1 p <@not %p @> r |> LR
        excluded_middle' |> L |> L'
        ident_conseq_true r |> Lemma
    ]

    let mutual_implication p q = ident prop_calculus <@ ((%p ==> %q) |&| (%q ==> %p)) = %p = %q @> [
        right_assoc |> LR
        ident_implies_not_or p q |> L
        ident_implies_not_or q p |> L  
        distrib |> L  
        commute |> L |> L' |> L'
        commute |> R |> L'
        distrib |> L |> L'
        distrib |> R |> L'
        distrib |> L |> L' |> L'
        commute |> L |> L'
        distrib |> L |> L'
        contr q |> CommuteL |> L
        contr p |> L
        ident_or <@ %p |&| %q @> |> CommuteL |> L
        ident_or <@ not %q |&| not %p @> |> CommuteL |> L
        commute |> L |> L'
        ident_eq_and_or_and p q |> Commute |> Lemma
        ]