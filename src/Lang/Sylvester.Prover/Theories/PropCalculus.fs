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
  
    let _mutual_implication = EquationalLogic._mutual_implication

    let _subst_and = EquationalLogic._subst_and

    let _subst_implies = EquationalLogic._subst_implies

    let _subst_and_implies = EquationalLogic._subst_and_implies

    let _subst_true = EquationalLogic._subst_true

    let _subst_false = EquationalLogic._subst_false

    let _subst_or_and = EquationalLogic._subst_or_and

    let _distrib_implies = EquationalLogic._distrib_implies

    let _double_neg = EquationalLogic._double_neg

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
    let excluded_middle = Theory.S.Rules.[7]

    /// Logical expression satisfies golden rule.
    let golden_rule = Theory.S.Rules.[8]

    let def_implies = Theory.S.Rules.[9]

    let shunt = Theory.S.Rules.[10]

    let rshunt = Theory.S.Rules.[11]

    let mutual_implication = Theory.S.Rules.[12]

    let subst_and = Theory.S.Rules.[13]

    let subst_implies = Theory.S.Rules.[14]

    let subst_and_implies = Theory.S.Rules.[15]

    let subst_true = Theory.S.Rules.[16]

    let subst_false = Theory.S.Rules.[17]

    let subst_or_and = Theory.S.Rules.[18]

    let distrib_implies = Theory.S.Rules.[19]

    let double_neg = Theory.S.Rules.[20]
     
    (* Tactics for rules *)

    /// The constant true is a theorem
    let Truth = Tactics.Truth commute

    /// If A is a theorem then so is A = true.
    let Taut :Theorem->Rule=  
        let ieq p = 
            let stmt = <@@ ((%%p) = true) = (%%p) @@> in Theorem(stmt, Proof (stmt, prop_calculus, [ApplyLeft commute; LR right_assoc], true)) |> Ident  
        Tactics.Taut ieq
    
    /// If A = B is a theorem then so is (A = B) = true.
    let Taut' t = 
        let ieq p = Theorem(<@@ ((%%p) = true) = (%%p) @@>, Proof (<@@ (%%p = true) = %%p @@>, prop_calculus, [ApplyLeft commute; LR right_assoc], true)) |> Ident 
        Tactics.Taut' ieq t
        
    let Lemma :Theorem->RuleApplication = Taut >> Truth >> LR
    
    let Lemma' = Taut' >> Truth >> LR

    /// If A = B is a theorem then so is B = A.
    let Commute = Tactics.Commute commute
    
    /// If (L = R) = B is a theorem then so is (R = L) = B.
    let CommuteL = Tactics.CommuteL commute

    /// If A = (L = R) is a theorem then so is A = (R = L).
    let CommuteR = Tactics.CommuteR commute

    let LeftAssoc = Tactics.LeftAssoc right_assoc

    let LeftAssocL = Tactics.LeftAssocL right_assoc

    let LeftAssocR = Tactics.LeftAssocR right_assoc

    let RightAssoc = Tactics.RightAssoc left_assoc

    let RightAssocL = Tactics.RightAssocL left_assoc

    let RightAssocR = Tactics.RightAssocR left_assoc

    (* Tactics for proofs *)
    
    let MutualImplication stmt = Tactics.MutualImplication prop_calculus Taut mutual_implication stmt

    (* Theorems *)
    
    /// true = (p = p)
    let def_true (p:Prop) = id_ax prop_calculus (true == (p == p))  
        
    /// false = (not p = p)
    let def_false (p:Prop) = ident prop_calculus (false == (!!p == p)) [
        R collect
        def_true p |> Commute |> R
    ] 

    /// (p = true) = p
    let ident_eq (p:Prop) = ident prop_calculus ((p == true) == p)  [
        ApplyLeft commute
        LR right_assoc
    ]

    /// p = q = q = p
    let commute_eq (p:Prop) (q:Prop) = ident prop_calculus ( (p == q) == (q == p) ) [LR left_assoc]

    /// p = (q = r) = p = q = r
    let left_assoc_eq (p:Prop) (q:Prop) (r:Prop) = ident prop_calculus ( (p == (q == r)) == ((p == q) == r) ) [R right_assoc]

    /// (p = q) = r = p = (q = r)
    let right_assoc_eq (p:Prop) (q:Prop) (r:Prop) = id_ax prop_calculus (((p == q) == r) == (p == (q == r)))

    /// not false = true
    let not_false = ident prop_calculus (!!F == T) [
        LR commute
        def_true F  |> ApplyLeft
        LR right_assoc
        R commute
        R collect
        def_true F |> Commute |> R  
    ]

    /// not not p = p
    let double_negation (p:Prop) = ident prop_calculus ((!!(!!p)) == p) [
         LR collect
         def_false p |> Commute |> LR
         not_false |> Truth |> LR
    ]

    /// not p = q = p = not q
    let symm_not_eq (p:Prop) (q:Prop) = ident prop_calculus (!!p == q == p == !!q) [
        collect |> ApplyLeft
        right_assoc |> LR
        collect |> ApplyLeft
        commute |> R
        collect |> R
        commute_eq q p |> R
    ]

    /// (p = q) = (not p = not q)
    let symm_eq_not_eq (p:Prop) (q:Prop) = ident prop_calculus (p == q == (!!p == !!q) ) [
        left_assoc |> LR
        commute_eq (p == q) !!p |> ApplyLeft
        commute_eq p q |> ApplyLeft
        left_assoc |> ApplyLeft
        symm_not_eq p q |> Lemma'
    ]

    /// ((p = q) = (r = s)) = ((p = r) = (q = s))
    let commute_eq_eq (p:Prop) (q:Prop) (r:Prop) (s:Prop) = ident prop_calculus (((p == q) == (r == s)) == ((p == r) == (q == s))) [
        left_assoc_eq (p == q) r s |> ApplyLeft
        right_assoc_eq p q r |> ApplyLeft
        commute_eq q r |> ApplyLeft
        left_assoc_eq p r q |> ApplyLeft
    ]

    /// p ||| q = q ||| p
    let commute_or (p:Prop) (q:Prop) = id_ax prop_calculus ((p + q) == (q + p))
 
    /// p ||| (q ||| r) = p ||| q ||| r
    let left_assoc_or (p:Prop) (q:Prop) (r:Prop) = ident prop_calculus ( (p + (q + r)) == ((p + q) + r) ) [LR left_assoc; LR commute]

    /// (p ||| q) ||| r = p ||| (q ||| r)
    let right_assoc_or p q r = left_assoc_or p q r |> Commute

    /// ((p ||| q) ||| (r ||| s)) = ((p ||| r) ||| (q ||| s))
    let commute_or_or (p:Prop) (q:Prop) (r:Prop) (s:Prop) = ident prop_calculus (((p + q) + (r + s)) == ((p + r) + (q + s))) [
        left_assoc_or (p + q) r s |> ApplyLeft
        right_assoc_or p q r |> ApplyLeft
        commute_or q r |> ApplyLeft
        left_assoc_or p r q |> ApplyLeft
    ]

    /// p ||| (q = r) = (p ||| q) = (p ||| r)
    let distrib_or_eq (p:Prop) (q:Prop) (r:Prop) = id_ax prop_calculus ((p + (q == r)) == ((p + q) == (p + r)))

    /// (p ||| q) = (p ||| r) = p ||| (q ||| r)
    let collect_or_eq p q r = distrib_or_eq p q r |> Commute

    /// (p ||| p) = p
    let idemp_or p =  id_ax prop_calculus ((p + p) == p) 

    /// p ||| true = true
    let zero_or p = ident prop_calculus ((p + T) == T) [
        def_true p |> R |> NextLeft
        distrib |> ApplyLeft
        commute |> LR
    ]

    /// p ||| false = p
    let ident_or (p:Prop) = ident prop_calculus ((p + F) == p) [
        def_false p |> R |> NextLeft
        ApplyLeft distrib
        LR right_assoc
        idemp_or p |> R
        ApplyLeft excluded_middle
    ]

    /// (p ||| q) = (p ||| not q = p)
    let ident_or_or_not (p:Prop) q = ident prop_calculus ((p + q) == ((p + !!q) == p)) [
        LR left_assoc
        collect_or_eq p q !!q  |> ApplyLeft
        commute_eq q !!q |> ApplyLeft
        def_false q  |> Commute |> ApplyLeft
        ident_or p  |> ApplyLeft      
    ]

    /// (p ||| not q) = (p = (p or q))
    let ident_or_not_or (p:Prop) (q:Prop) = ident prop_calculus ((p + !!q) == (p == (p + q))) [
        commute |> R
        LR left_assoc
        collect_or_eq p !!q q  |> ApplyLeft
        def_false q |> Commute |> ApplyLeft
        ident_or p |> ApplyLeft
    ]

    
    /// p ||| (q ||| r) = ((p ||| q) ||| (p ||| r))
    let distrib_or_or (p:Prop) (q:Prop) (r:Prop) =  ident prop_calculus ((p + (q + r)) == ((p + q) + (p + r))) [
        idemp_or p |> Commute |> ApplyLeft
        right_assoc |> ApplyLeft
        left_assoc_or p q r |> ApplyLeft
        commute_or p q |> ApplyLeft
        right_assoc_or q p r |> ApplyLeft
        left_assoc |> ApplyLeft
    ]

    /// (p ||| q) = (p ||| r) = p ||| (q ||| r)
    let collect_or_or p q r = distrib_or_or p q r |> Commute

    /// not (p = q) = not p = q
    let distrib_not (p:Prop) (q:Prop) = ident prop_calculus ((!!(p == q)) == (!!p == q)) [LR right_assoc]

    /// (not p = q) = not (p = q) 
    let collect_not p q = distrib_not p q |> Commute

    /// p <> q = not (p = q)
    let def_not_eq (p:Prop) (q:Prop) = ident prop_calculus ((p != q) == (!!(p == q))) [
        right_assoc |> LR
    ]

    /// p <> q = q <> p
    let commute_not_eq (p:Prop) (q:Prop) = ident prop_calculus ((p != q) == (q != p)) [
        def_not_eq p q |> ApplyLeft
        def_not_eq q p |> R
        commute_eq q p |> R
    ]

    /// (p <> q) <> r = p <> (q <> r)
    let right_assoc_not_eq p q r = ident prop_calculus (((p != q) != r) == (p != (q != r))) [
        def_not_eq p q  |> ApplyLeft
        def_not_eq (!!(p == q)) r |> ApplyLeft
        def_not_eq q r |> R
        def_not_eq p (!!(q == r)) |> R
        distrib_not q r |> R
        left_assoc_eq p !!q r |> R
        commute_eq p !!q |> R
        collect_not q p |> R
        commute_eq q p |> R
    ]

    /// p <> (q <> r) = (p <> q) <> r  
    let left_assoc_not_eq p q r = right_assoc_not_eq p q r |> Commute

    
    /// p ||| not p = true
    let excluded_middle' (p:Prop) = ident prop_calculus ((p + (!!p)) == T) [ident_eq (p + (!!p)) |> LR]
    
    /// p |&| q = ((p = q) = (p ||| q))
    let golden_rule' (p:Prop) (q:Prop) = id_ax prop_calculus ((p * q) == (p == q == (p + q)))
    (*
    /// p |&| q = q |&| p
    let commute_and p q = ident prop_calculus <@ (%p |&| %q) = (%q |&| %p) @> [
        golden_rule' p q |> ApplyLeft
        golden_rule' q p |> R
        commute_or q p |> R
        commute_eq q p |> R 
    ]

    /// p |&| q |&| r = (p = q = r = (p ||| q) = (q ||| r) = (r ||| p) = (p ||| q ||| r))
    let ident_and_eq_all p q r = ident prop_calculus <@ (%p |&| %q |&| %r) = (%p = %q = %r = (%p ||| %q) = (%q ||| %r) = (%r ||| %p) = (%p ||| %q ||| %r)) @> [
        golden_rule' p q |> ApplyLeft
        golden_rule' <@ (%p = %q) = (%p ||| %q) @> r |> ApplyLeft 
        commute_or <@ ((%p = %q) = (%p ||| %q)) @> r |> ApplyLeft
        distrib_or_eq r <@ %p = %q @> <@ %p ||| %q @> |> ApplyLeft
        distrib_or_eq r p q |> ApplyLeft
        right_assoc_eq <@ %p = %q @> <@ %p ||| %q @> r |> ApplyLeft
        commute_eq <@ %p ||| %q @> r |> ApplyLeft
        commute_or r q |> ApplyLeft
        commute_eq <@ %r ||| %p @> <@ %q ||| %r @> |> ApplyLeft
        commute_or r <@ %p ||| %q @> |> ApplyLeft
        left_assoc_eq <@ %p = %q @> r <@ %p ||| %q @> |> ApplyLeft
        left_assoc |> ApplyLeft
        left_assoc_eq <@ %p = %q = %r = (%p ||| %q) @> <@ %q ||| %r @> <@ %r ||| %p @> |> ApplyLeft
    ]
    
    /// p |&| q |&| r = p |&| (q |&| r)
    let right_assoc_and p q r = ident prop_calculus <@ (%p |&| %q |&| %r) = (%p |&| (%q |&| %r)) @> [
        ident_and_eq_all p q r |> ApplyLeft
        commute_and p <@ %q |&| %r @> |> R
        ident_and_eq_all q r p |> R
        commute_eq <@ %q = %r @> p |> R
        left_assoc_eq <@ %p = %q = %r = (%p ||| %q) @> <@ %q ||| %r @> <@ %r ||| %p @> |> ApplyLeft
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
        L golden_rule
        right_assoc |> LR
        idemp_or p |> Taut' |> R
        LR commute 
    ] 
    
    /// p |&| true = p
    let ident_and p = ident prop_calculus <@ (%p |&| true) = %p @> [
        L golden_rule
        LR right_assoc
        zero_or p |> R
        R commute
    ]

    /// p |&| false = false
    let zero_and p = ident prop_calculus <@ (%p |&| false) = false @> [
      golden_rule' p <@ false @> |> ApplyLeft
      ident_or p |> ApplyLeft
      LR right_assoc
    ]

    /// p |&| (q |&| r) = (p |&| q) |&| (p |&| r)
    let distrib_and p q r = ident prop_calculus <@ (%p |&| (%q |&| %r)) = ((%p |&| %q) |&| (%p |&| %r)) @> [
        idemp_and p |> Commute |> ApplyLeft |> ApplyLeft'
        right_assoc |> ApplyLeft
        left_assoc_and p q r |> R |> ApplyLeft'
        commute_and p q |> R |> ApplyLeft'
        right_assoc_and q p r |> R |> ApplyLeft'
        left_assoc |> ApplyLeft
    ]

    /// p |&| not p = false
    let contr p = ident prop_calculus <@ %p |&| not %p = false@> [
        golden_rule |> ApplyLeft
        excluded_middle |> R |> ApplyLeft'
        commute_eq p <@ not %p @> |> ApplyLeft
        def_false p |> Commute |> ApplyLeft
        commute_eq <@ false @> <@ true @> |> ApplyLeft
        right_assoc |> LR
    ]

    /// (p |&| (p ||| q)) = p
    let absorb_and p q = ident prop_calculus <@ (%p |&| (%p ||| %q)) = %p @> [
        L golden_rule
        left_assoc_or p  p  q |> ApplyLeft
        idemp_or p |> ApplyLeft
    ]

    /// (p ||| (p |&| q)) = p
    let absorb_or p q = ident prop_calculus <@ (%p ||| (%p |&| %q)) = %p @> [
        golden_rule |> R |> ApplyLeft'
        distrib |> ApplyLeft
        left_assoc_or p p q |> ApplyLeft
        idemp_or p |> ApplyLeft
        distrib_or_eq p p q |> ApplyLeft
        idemp_or p |> ApplyLeft
    ]

    /// p |&| (not p ||| q) = (p |&| q)
    let absorb_and_not p q = ident prop_calculus <@ %p |&| (not %p ||| %q) = (%p |&| %q) @> [
        golden_rule |> ApplyLeft
        left_assoc_or p <@ not %p @> q |> ApplyLeft
        excluded_middle' p |> ApplyLeft
        zero_or q |> CommuteL |> ApplyLeft
        ident_eq <@ %p = (not %p ||| %q) @> |> ApplyLeft
        commute_or <@ not %p @> q |> ApplyLeft
        ident_or_not_or q p |> ApplyLeft
        left_assoc |> ApplyLeft
        commute_or q p |> ApplyLeft
        golden_rule' p q |> Commute |> ApplyLeft
    ]

    /// p ||| (not p |&| q) = (p ||| q)
    let absorb_or_not p q = ident prop_calculus <@ %p ||| (not %p |&| %q) = (%p ||| %q) @> [
        golden_rule |> ApplyLeft
        commute_or <@ not %p @> q  |> ApplyLeft
        right_assoc_eq <@ not %p @> q  <@ %q ||| not %p @> |> ApplyLeft
        ident_or_or_not q  p |> Commute |> CommuteL |> ApplyLeft
        distrib |> ApplyLeft 
        commute_or q p |> ApplyLeft
        left_assoc_or p p q |> ApplyLeft
        idemp_or p |> ApplyLeft
        excluded_middle |> ApplyLeft
        ident_eq <@ %p ||| %q @> |> CommuteL |> ApplyLeft
    ]
    
    /// p ||| (q |&| r) = ((p ||| q) |&| (p ||| r))
    let distrib_or_and p q r = ident prop_calculus <@ %p ||| (%q |&| %r) = ((%p ||| %q) |&| (%p ||| %r)) @> [
        golden_rule |> R |> ApplyLeft'
        distrib |> ApplyLeft
        distrib |> ApplyLeft |> ApplyLeft'
        distrib_or_or p q r |> ApplyLeft
        golden_rule' <@ %p ||| %q @> <@ %p ||| %r @> |> Commute |> ApplyLeft
    ]

    /// ((p ||| q) |&| (p ||| r)) = p ||| (q |&| r)
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
        golden_rule |> LR |> LR' |> ApplyLeft'
        distrib |> ApplyLeft
        distrib |> ApplyLeft |> ApplyLeft' 
        ident_or_or_not <@ not %p @> <@ not %q @> |> R
        double_negation q |> R
        ident_or_not_or q p |> CommuteL |> R
        commute |> R
        commute_or q p |> R
    ]

    /// not p ||| not q = not (p |&| q) 
    let collect_not_and p q = distrib_not_and p q |> Commute

    /// not (p ||| q) = not p |&| not q
    let distrib_not_or p q = ident prop_calculus <@ not (%p ||| %q) = (not %p |&| not %q) @> [
        golden_rule' p q |> Commute |> CommuteL |> RightAssoc |> ApplyLeft
        commute |> LR |> LR' |> ApplyLeft'
        distrib |> ApplyLeft
        distrib_not_and p q |> ApplyLeft
        commute |> LR
        symm_eq_not_eq p q |> R
        commute |> R
    ]

    /// not p |&| not q = not (p ||| q)
    let collect_not_or p q = distrib_not_or p q |> Commute
    
    /// p ||| q = (p ||| not q = p)
    let ident_or_or_not_eq p q = ident prop_calculus <@ (%p ||| %q) = (%p ||| not %q = %p) @> [
        left_assoc |> LR
        collect_or_eq p q <@ not %q @> |> LR
        commute_eq q <@ not %q @> |> ApplyLeft
        def_false q |> Commute |> ApplyLeft
        ident_or p |> ApplyLeft
    ]

    /// p = q = ((p |&| q) ||| (not p |&| not q))
    let ident_eq_and_or_not p q = ident prop_calculus <@ %p = %q = ((%p |&| %q) ||| (not %p |&| not %q))@> [
        ident_or_or_not <@ %p |&| %q @> <@ not %p |&| not %q @> |> R
        distrib_not_and <@ not %p @> <@ not %q @> |> R
        double_negation p |> R
        double_negation q |> R
        distrib |> ApplyLeft |> R'
        absorb_or p q |> CommuteL |> R
        commute_and p q |> R
        absorb_or q p |> CommuteL |> R
        commute_and q p |> R
        left_assoc |> LR
        commute |> LR
    ]

    /// p |&| q = (p |&| not q = not p)
    let ident_and_and_not p q = ident prop_calculus <@ (%p |&| %q) = (%p |&| not %q = not %p) @> [
        left_assoc |> LR
        golden_rule |> ApplyLeft |> ApplyLeft'
        golden_rule' p <@ not %q @> |> ApplyLeft
        commute |> R |> ApplyLeft'
        left_assoc |> R |> ApplyLeft'
        ident_or_or_not_eq p q |> Commute |> ApplyLeft
        left_assoc |> ApplyLeft
        right_assoc |> ApplyLeft |> ApplyLeft'
        def_true <@ %p ||| %q @> |> Commute |> ApplyLeft
        commute |> ApplyLeft |> ApplyLeft'
        right_assoc |> LR
        commute |> R
        right_assoc |> LR
        symm_eq_not_eq p q |> R
    ]

    /// p |&| (q = r) = ((p |&| q) = (p |&| r) = p)
    let distrib_and_eq p q r = ident prop_calculus <@ %p |&| (%q = %r) = ((%p |&| %q) = (%p |&| %r) = %p) @> [
        golden_rule |> ApplyLeft
        distrib |> R |> ApplyLeft'
        left_assoc |> ApplyLeft |> ApplyLeft'
        left_assoc |> ApplyLeft
        right_assoc |> ApplyLeft
        commute_eq_eq <@ %p = %q @> r <@ %p ||| %q @> <@ %p ||| %r @> |> ApplyLeft
        golden_rule' p q |> LeftAssoc |> ApplyLeft
        golden_rule' p r |> LeftAssoc |> LeftAssocL |> RightAssoc |> Commute |> ApplyLeft 
        golden_rule' p q |> Commute |> ApplyLeft
        left_assoc |> ApplyLeft
    ]

    /// p |&| (q = p) = (p |&| q)
    let ident_and_eq p q  = ident prop_calculus <@ %p |&| (%q = %p) = (%p |&| %q) @> [
        golden_rule |> ApplyLeft
        distrib |> R |> ApplyLeft'
        left_assoc |> ApplyLeft |> ApplyLeft'
        left_assoc |> ApplyLeft
        right_assoc |> ApplyLeft
        idemp_or p |> ApplyLeft
        commute |> ApplyLeft |> ApplyLeft'
        left_assoc |> ApplyLeft |> ApplyLeft'
        def_true p |> Commute |> ApplyLeft |> ApplyLeft'
        ident_eq q |> CommuteL |> ApplyLeft
        commute |> ApplyLeft
        golden_rule' p q |> Commute |> CommuteL |> LeftAssocL |> ApplyLeft
    ]

    /// p |&| q |&| (r |&| s) = p |&| r |&| (q |&| s) 
    let commute_and_and p q r s =  ident prop_calculus <@ ((%p |&| %q) |&| (%r |&| %s)) = ((%p |&| %r) |&| (%q |&| %s)) @> [
        right_assoc_and p q <@ %r |&| %s @> |> ApplyLeft
        left_assoc_and q r s |>  L
        commute_and q r |> ApplyLeft
        right_assoc_and r q s |> ApplyLeft
        left_assoc_and p r <@ %q |&| %s @> |> ApplyLeft
    ]

    /// p ==> q = (p ||| q = q)
    let def_implies' p q = id_ax prop_calculus <@ (%p ==> %q) = (%p ||| %q = %q) @>

    /// p ==> q = (not p ||| q)
    let ident_implies_not_or p q = ident prop_calculus <@ %p ==> %q = (not %p ||| %q) @> [
        def_implies |> ApplyLeft
        ident_or_not_or q p |> CommuteL |> R
        commute |> R
        commute |> ApplyLeft |> R'
    ]

    /// p ==> q = ((p |&| q) = p)
    let ident_implies_eq_and_eq p q = ident prop_calculus <@ %p ==> %q = ((%p |&| %q) = %p) @> [
        def_implies |> ApplyLeft
        commute |> LR
        right_assoc |> LR
        commute |> R |> R' 
        left_assoc |> R 
    ]

    /// p |&| (p ==> q) = (p |&| q)
    let ident_and_implies p q = ident prop_calculus <@ %p |&| (%p ==> %q) = (%p |&| %q) @> [
        ident_implies_eq_and_eq p q |> ApplyLeft
        distrib_and_eq p <@ %p |&| %q @> p |> ApplyLeft
        left_assoc |> ApplyLeft |> ApplyLeft' |> ApplyLeft'
        idemp_and p |> ApplyLeft
    ]

    /// p ||| (q ==> p) = (q ==> p)
    let ident_or_conseq p q = ident prop_calculus <@ %p ||| (%q ==> %p) = (%q ==> %p) @> [
        def_implies |> R |> ApplyLeft'
        distrib |> ApplyLeft
        commute_or q p |> ApplyLeft
        left_assoc_or p p q |> ApplyLeft 
        idemp_or p |> ApplyLeft
        commute |> LR
        commute_or p q |> R
    ]

    /// p ==> q = (not q ==> not p)
    let def_implies_contr p q = ident prop_calculus <@ %p ==> %q = (not %q ==> not %p) @> [
        def_implies |> R
        commute |> R
        commute |> R |> R'
        distrib_not_and p q |> Commute |> R |> R'
        symm_eq_not_eq p <@ %p |&| %q @> |> Commute |> R 
        commute |> R
        ident_implies_eq_and_eq p q |> Lemma'
    ]

    /// p ==> (q = r) = ((p |&| q) = (p |&| r))
    let distrib_implies_eq_and p q r =
        ident prop_calculus <@ %p ==> (%q = %r) = ((%p |&| %q) = (%p |&| %r))@> [
            ident_implies_eq_and_eq p <@ %q = %r @> |> ApplyLeft
            distrib_and_eq p q r |> ApplyLeft
    ]

    /// p ==> (q = r) = ((p ==> q) = (p ==> r))
    let distrib_implies_eq_implies p q r = ident prop_calculus <@ %p ==> (%q = %r) = ((%p ==> %q) = (%p ==> %r))@> [
        distrib_implies_eq_and p q r |> ApplyLeft
        ident_implies_eq_and_eq p q |> ApplyLeft |> R'
        ident_implies_eq_and_eq p r |> R |> R'
        commute |> R |> R'
        left_assoc |> R 
        right_assoc |> ApplyLeft |> R'
        def_true p |> Commute |> R
        ident_eq <@ %p |&| %q @> |> ApplyLeft |> R'
    ]

    /// p ||| (p ==> q)
    let or_implies p q = theorem prop_calculus <@ (%p ||| (%p ==> %q)) = true @> [
        def_implies |> R |> ApplyLeft'
        distrib |> ApplyLeft
        left_assoc |> ApplyLeft |> ApplyLeft'
        idemp_or p |> ApplyLeft
        ident_eq <@ (%p ||| %q) = (%p ||| %q) @> |> LR
    ]

    /// p ==> p
    let reflex_implies p = theorem prop_calculus <@ %p ==> %p @> [
        def_implies |> LR
    ]
        
    /// p ==> true
    let implies_true p = theorem prop_calculus <@ %p ==> true @> [
        def_implies |> LR
        zero_or p |> ApplyLeft
    ]

    /// false ==> p
    let conseq_false p = theorem prop_calculus <@ false ==> %p @> [
        def_implies |> LR
        ident_or p |> CommuteL |> Lemma'
    ]

    /// (true ==> p) = p
    let ident_conseq_true p = ident prop_calculus <@ (true ==> %p) = %p @> [
        def_implies |> ApplyLeft
        zero_or p |> CommuteL |> ApplyLeft
        right_assoc |> LR
        commute |> LR
    ]

    /// p ==> false = (not p)
    let ident_implies_false_not p = ident prop_calculus <@ (%p ==> false) = not %p @> [
        def_implies |> ApplyLeft
        ident_or p |> ApplyLeft
        commute |> LR
        left_assoc |> LR
        commute |> LR
        def_false p |> LR
    ]
    
    /// p |&| q ==> r = (p ==> (q ==> r))
    let shunt' p q r = ident prop_calculus <@ %p |&| %q ==> %r = (%p ==> (%q ==> %r)) @> [
        ident_implies_eq_and_eq <@ %p |&| %q @> r |> ApplyLeft
        ident_implies_eq_and_eq q r |> R
        ident_implies_eq_and_eq p <@ %q |&| %r = %q @> |> R
        distrib_and_eq p <@ %q |&| %r @> q |> R
        left_assoc_and p q r |> R
        right_assoc |> R
        def_true p |> Commute |> R
        left_assoc |> LR
        commute |> LR
    ]

    /// (p |&| q) ==> p
    let strengthen_and p q = theorem prop_calculus <@ (%p |&| %q ) ==> %p @> [
        ident_eq <@ ((%p |&| %q ) ==> %p) @> |> LR
        def_implies |> LR
        commute |> ApplyLeft
        absorb_or p q |> Lemma'
    ]
    
    /// p ==> p ||| q 
    let weaken_or p q = theorem prop_calculus <@ %p ==> (%p ||| %q) @> [
        ident_eq <@ (%p ==> (%p ||| %q)) @> |> LR
        def_implies |> LR
        left_assoc |> ApplyLeft
        idemp_or p |> ApplyLeft
    ]

    /// p |&| q ==> p ||| q
    let weaken_and_or p q = theorem prop_calculus <@ %p |&| %q ==> %p ||| %q @> [
        def_implies |> ApplyLeft
        commute |> ApplyLeft |> ApplyLeft'
        distrib |> ApplyLeft |> ApplyLeft'
        commute |> LR
        idemp_or p |> LR
        distrib |> ApplyLeft |> R'
        idemp_and p |> LR
        distrib |> LR
        distrib |> R |> ApplyLeft'
        distrib |> ApplyLeft
        idemp_or p |> ApplyLeft
        distrib |> ApplyLeft
        commute_or q p |> ApplyLeft
        idemp_and <@ %p ||| %q @> |> ApplyLeft
        commute |> ApplyLeft |> ApplyLeft'
        distrib |> ApplyLeft |> ApplyLeft'
        idemp_and q |> ApplyLeft
        absorb_or q p |> CommuteL |> ApplyLeft
        commute |> R |> ApplyLeft'
        left_assoc |> ApplyLeft
        idemp_or q |> ApplyLeft
    ]

    /// (p ||| (q |&| r)) ==> (p ||| q)
    let weaken_or_and p q r = theorem prop_calculus <@ (%p ||| (%q |&| %r)) ==> (%p ||| %q) @> [
        distrib |> ApplyLeft
        strengthen_and <@ %p ||| %q @> <@ %p ||| %r @> |> Lemma
    ]

    /// (p |&| q) ==> (p |&| (q ||| r))
    let weaken_and_and_or p q r = theorem prop_calculus <@ (%p |&| %q)  ==> (%p |&| (%q ||| %r)) @> [
        distrib |> R
        weaken_or <@ %p |&| %q @> <@ %p |&| %r @> |> Lemma
    ]

    /// p |&| (p ==> q) ==> q
    let modus_ponens p q = theorem prop_calculus <@ %p |&| (%p ==> %q) ==> %q @> [
        ident_and_implies p q |> ApplyLeft
        commute_and p q |> LR
        strengthen_and q p |> Lemma
    ]

    /// (p ==> r) |&| (q ==> r) = (p ||| q ==> r)
    let case_analysis_1 p q r = ident prop_calculus <@( %p ==> %r) |&| (%q ==> %r) = (%p ||| %q  ==> %r) @> [
        ident_implies_not_or <@ %p ||| %q @> r |> R
        distrib|> ApplyLeft |> R'
        distrib_or_and r <@ not %p @> <@ not %q @> |> CommuteL |> R
        commute |> ApplyLeft |> R'
        commute |> R |> R'
        ident_implies_not_or p r |> Commute |> R
        ident_implies_not_or q r |> Commute |> R
    ]

    /// (p ==> r) |&| (not p ==> r) = r
    let case_analysis_2 p r = ident prop_calculus <@ (%p ==> %r) |&| (not %p ==> %r) = %r @> [
        case_analysis_1 p <@not %p @> r |> LR
        excluded_middle |> ApplyLeft |> ApplyLeft'
        ident_conseq_true r |> Lemma'
    ]

    /// (p ==> q) |&| (q ==> p) = (p = q)
    let mutual_implication' p q = ident prop_calculus <@ ((%p ==> %q) |&| (%q ==> %p)) = (%p = %q) @> [
        right_assoc |> LR
        ident_implies_not_or p q |> ApplyLeft
        ident_implies_not_or q p |> ApplyLeft  
        distrib |> ApplyLeft  
        commute |> ApplyLeft |> ApplyLeft' |> ApplyLeft'
        commute |> R |> ApplyLeft'
        distrib |> ApplyLeft |> ApplyLeft'
        distrib |> R |> ApplyLeft'
        distrib |> ApplyLeft |> ApplyLeft' |> ApplyLeft'
        commute |> ApplyLeft |> ApplyLeft'
        distrib |> ApplyLeft |> ApplyLeft'
        contr q |> CommuteL |> ApplyLeft
        contr p |> ApplyLeft
        ident_or <@ %p |&| %q @> |> CommuteL |> ApplyLeft
        ident_or <@ not %q |&| not %p @> |> CommuteL |> ApplyLeft
        commute |> ApplyLeft |> ApplyLeft'
        commute |> LR
        commute |> R
        ident_eq_and_or_not p q |> ApplyLeft
    ]

    /// (p ==> q) |&| (q ==> p) ==> (p = q)
    let antisymm_implies p q = theorem prop_calculus <@ (%p ==> %q) |&| (%q ==> %p) ==> (%p = %q) @> [
        mutual_implication' p q |> ApplyLeft  
        reflex_implies <@ p = q @> |> Lemma
    ]

    /// (p ==> q) |&| (q ==> r) ==> (p ==> r)
    let trans_implies p q r = theorem prop_calculus <@ (%p ==> %q) |&| (%q ==> %r) ==> (%p ==> %r) @> [
        rshunt |> LR
        commute |> ApplyLeft
        left_assoc |> ApplyLeft
        ident_and_implies p q |> ApplyLeft
        right_assoc |> ApplyLeft
        ident_and_implies q r |> ApplyLeft
        commute |> ApplyLeft
        commute |> ApplyLeft |> ApplyLeft'
        right_assoc |> ApplyLeft
        strengthen_and r <@ %q |&| %p @> |> Lemma
    ]

    /// (p = q) |&| (q ==> r) ==> (p ==> r)
    let trans_implies_eq p q r = theorem prop_calculus <@ (%p = %q) |&| (%q ==> %r) ==> (%p ==> %r) @> [
        mutual_implication' p q |> Commute |> ApplyLeft
        rshunt |> LR
        commute |> ApplyLeft
        left_assoc |> ApplyLeft
        left_assoc |> ApplyLeft |> ApplyLeft'
        ident_and_implies p q |> ApplyLeft |> ApplyLeft'
        right_assoc |> ApplyLeft |> ApplyLeft'
        ident_and_implies q p |> ApplyLeft |> ApplyLeft'
        commute |> R |> ApplyLeft' |> ApplyLeft'
        left_assoc |> ApplyLeft |> ApplyLeft'
        idemp_and p |> ApplyLeft |> ApplyLeft'
        right_assoc |> ApplyLeft
        ident_and_implies q r |> ApplyLeft
        left_assoc |> ApplyLeft
        commute |> ApplyLeft
        strengthen_and r <@ %p |&| %q @> |> Lemma
    ]

    /// p ==> (q ==> p)
    let trans_implies_implies p q = theorem prop_calculus <@ %p ==> (%q ==> %p) @> [
        def_implies |> R
        def_implies |> LR
        commute |> ApplyLeft |> R' |> ApplyLeft' 
        distrib |> ApplyLeft
        left_assoc |> ApplyLeft |> ApplyLeft'
        idemp_or p |> ApplyLeft |> ApplyLeft' |> ApplyLeft'
        commute |> ApplyLeft |> ApplyLeft' 
        idemp_or p |> R |> ApplyLeft'
    ]

    /// (p ==> q) ==> ((p ||| r) ==> (q ||| r)
    let mono_or p q r = theorem prop_calculus <@ (%p ==> %q) ==> ((%p ||| %r) ==> (%q ||| %r)) @> [
        def_implies |> R
        commute_or_or p r q r |> ApplyLeft |> R'
        idemp_or r |> ApplyLeft |> R'
        commute_or <@ %p ||| %q @> r |> ApplyLeft |> R'
        commute_or q r |> R |> R'
        collect_or_eq r <@ %p ||| %q @> q |> R
        commute |> R
        def_implies' p q |> Commute |> R
        weaken_or <@ %p ==> %q @> r |> Lemma
    ]
    *)