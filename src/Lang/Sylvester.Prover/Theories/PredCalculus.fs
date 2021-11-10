namespace Sylvester

open FSharp.Quotations
open PropCalculus

/// Predicate calculus using the axioms and rules of S.
module PredCalculus =
    let pred_calculus = Theory.S

    (* Expression functions for admissible rules *)
    
    let _empty_range = EquationalLogic._empty_range

    let _trade_body = EquationalLogic._trade_body

    let _distrib_or_forall = EquationalLogic._distrib_or_forall
    
    let fail_if_occurs_free x q = 
        do if Patterns.occurs_free (x |> get_vars) q then failwithf "One of the variables in %s occurs free in the quantifier %s." (src x) (src q)

    (* Admissible rules *)
    
    let empty_range = Theory.S.Rules.[21] 

    let trade_body = Theory.S.Rules.[22]

    let collect_forall_and = Theory.S.Rules.[23]

    let collect_exists_or = Theory.S.Rules.[24]

    let distrib_or_forall = Theory.S.Rules.[25]

    let split_range_forall = Theory.S.Rules.[26]

    let split_range_exists = Theory.S.Rules.[27]

    (* Theorems *)

    /// forall x N P = (forall x true (N ==> P))
    let trade_forall_implies x N P = id_ax pred_calculus <@ forall %x %N %P = (forall %x true (%N ==> %P)) @>
        
    /// forall x (Q |&| N) P = (forall x Q (N ==> P))
    let trade_forall_and_implies x Q N P = ident pred_calculus <@ forall %x (%Q |&| %N) %P = (forall %x %Q (%N ==> %P)) @> [
        trade_forall_implies x <@ %Q |&| %N @> P |> L
        shunt |> QB |> L'
        trade_forall_implies x Q  <@ %N==> %P @> |> Commute |> L
    ]

    /// forall x N P = (P ||| forall x true (not N))
    let trade_forall_or_not x N P = ident pred_calculus <@ forall %x %N %P = (%P ||| forall' %x (not %N)) @> [
        distrib_or_forall |> R
        commute_or P <@ not %N @> |> R
        ident_implies_not_or N P |> Commute |> R 
    ]

    /// forall x (N1 ||| N2) P = ((forall x N1 P) |&| (forall x N2 P))
    let split_range_forall' x N1 N2 P = id_ax pred_calculus <@ forall %x (%N1 ||| %N2) P = ((forall %x %N1 P) |&| (forall %x %N2 P)) @>
    
    /// P ||| forall x N Q = (forall x N (P ||| Q))
    let distrib_or_forall' x N P Q = id_ax pred_calculus <@ %P ||| forall %x %N %Q = (forall %x %N (%P ||| %Q)) @>

    /// ((forall x N P) |&| (forall x N Q)) = (forall x N (P |&| Q)) 
    let collect_forall_and' x N P Q = id_ax pred_calculus <@ ((forall %x %N %P) |&| (forall %x %N %Q)) = (forall %x %N (%P |&| %Q)) @>

    /// (forall x N (P |&| Q))  = ((forall x N P) |&| (forall x N Q))
    let distrib_forall_and' x N P Q = collect_forall_and' x N P Q |> Commute

    /// not (forall x true (not N)) ==> (forall x N (P |&| Q) = (P |&| forall x N Q))
    let distrib_forall_and_cond x N P Q = ident pred_calculus <@ not (forall' %x (not %N)) ==> (forall %x %N (%P |&| %Q) = (%P |&| forall %x %N %Q)) @> [
        let lemma1 = theorem pred_calculus <@ not (forall' %x (not %N)) ==> (forall' %x (not %N) = false) @> [
            distrib_implies_eq_and <@ (not (forall' %x (not %N))) @> <@ forall' %x (not %N) @> <@ false @> |> LR
            contr <@ forall' %x (not %N) @> |> CommuteL |> L
            zero_and <@ not (forall' %x (not %N)) @> |> R   
        ]
        distrib_forall_and' x N P Q |> R
        trade_forall_or_not x N P |> R
        deduce' lemma1 |> R
        ident_or P |> R
        def_true <@ %P |&| forall %x %N %Q @> |> Commute |> R
    ]

    /// forall x N true = true
    let ident_forall_true x N = ident pred_calculus <@ forall %x %N true = true @> [
        trade_forall_implies x N <@ true @> |> apply_left
        implies_true  N |> Taut |> apply_left
        trade_forall_or_not x <@ true @> <@ true @> |> apply_left
        commute |> apply_left
        zero_or <@ forall' %x (not true ) @> |> apply_left
    ]

    /// forall x true true = true
    let ident_forall_true' x = ident_forall_true x <@ true @>

    /// forall x N (P = Q) ==> (forall x N P = (forall x N Q))
    let distrib_forall_body x N P Q = theorem pred_calculus <@ forall %x %N (%P = %Q) ==> (forall %x %N %P  = (forall %x %N %Q)) @> [
        distrib_implies_eq_and <@ forall %x %N (%P = %Q) @> <@ forall %x %N %P @> <@ forall %x %N %Q @> |> LR
        collect_forall_and |> L
        collect_forall_and |> R
        commute_and <@ %P = %Q @> P |> L
        commute_and <@ %P = %Q @> Q |> R
        commute_eq P Q |> L
        ident_and_eq P Q |> L
        ident_and_eq Q P |> R
        commute_and Q P |> R
    ]

    /// forall x (R1 ||| R2) P ==> (forall x R1 P)
    let strengthen_forall_range_or x R1 R2 P = theorem pred_calculus <@ (forall %x (%R1 ||| %R2) %P) ==> (forall %x %R1 %P) @> [
        split_range_forall |> L
        strengthen_and <@ forall %x %R1 %P @> <@ forall %x %R1 %P @> |> Lemma
    ]

    /// forall x N (P |&| Q) ==> (forall x N P)
    let strengthen_forall_body_and x N P Q = theorem pred_calculus <@ (forall %x %N (%P |&| %Q)) ==> (forall %x %N %P) @> [
        distrib_forall_and' x N P Q |> L
        strengthen_and <@ forall %x %N %P @> <@ forall %x %N %Q @> |> Lemma
    ]

    /// forall x N (Q ==> P) ==> ((forall x N Q) ==> (forall x N P))
    let mono_forall_body x N Q P = theorem pred_calculus <@ forall %x %N (%Q ==> %P) ==> ((forall %x %N %Q) ==> (forall %x %N %P))@> [
        rshunt |> LR
        collect_forall_and |> L
        commute_and <@ %Q ==> %P @> Q |> L
        ident_and_implies Q P |> L
        commute_and Q P |> L
        strengthen_forall_body_and x N P Q |> Lemma
    ]

    /// P ==> forall' %x %P
    let forall_conseq_inst' x P = theorem pred_calculus <@ %P ==> forall' %x %P @> [
        axiom pred_calculus <@ %P ==> %P @> |> Deduce |> R
        ident_forall_true' x |> R
    ]

    /// forall x N P ==> (N ==> P)
    (*
    let forall_implies_inst x N P = theorem pred_calculus <@ forall %x %N %P ==> (%N ==> %P) @> [
        trade_body |> L
        inst' x <@ %N ==> %P @> |> L
        trade_forall_implies x N P |> Commute |> L
    ]
    *)
    /// N ==> P ==> forall x N P
    let forall_conseq_inst x N P = theorem pred_calculus <@ %N ==> %P ==> (forall %x %N %P) @> [
        trade_body |> R
        forall_conseq_inst' x <@ %N ==> %P @> |> Lemma
        trade_forall_implies x N P |> Commute |> R
    ]

    (*
    /// forall x N P = (N ==> P)
    let ident_forall_inst x N P = ident pred_calculus <@ forall %x %N %P = (%N ==> %P) @> [
        mutual_implication |> LR
        forall_conseq_inst x N P |> Taut |> R
        forall_implies_inst x N P |> Taut |> L
        idemp_and <@ true @> |> Truth |> LR
    ]
    *)
    /// exists x N P = not (forall x N (not P))
    let ident_exists_not_forall x N P = id_ax pred_calculus <@ exists %x %N %P = not (forall %x %N (not %P)) @>

    /// not (exists x N (not P)) = forall x N P
    let ident_not_exists_forall x N P = ident pred_calculus <@ not (exists %x %N (not %P)) = forall %x %N %P @> [
        ident_exists_not_forall x N <@ not %P @> |> L 
        double_negation P |> L 
        double_negation <@ forall %x %N %P @> |> L
    ]

    /// not (exists x N P) = forall x N (not P)
    let ident_not_exists_forall_not x N P = ident pred_calculus <@ not (exists %x %N %P) = forall %x %N (not %P) @> [
        ident_exists_not_forall x N P |> L 
        double_negation <@ forall %x %N (not %P) @> |> L 
    ]

    /// not (exists' x P) = forall' x (not P)
    let ident_not_exists_forall_not' x P = ident_not_exists_forall_not x <@ true @> P

    /// exists x N (not P) = not (forall x N P)
    let ident_exists_not_forall_not x N P = ident pred_calculus <@ exists %x %N (not %P) = not (forall %x %N %P) @> [
        ident_not_exists_forall x N P |> Commute |> R
        double_negation <@ exists %x %N (not %P) @> |> R
    ]

    /// exists x N P = exists true x (N |&| P)
    let trade_exists_and x N P = ident pred_calculus <@ exists %x %N %P = (exists' %x (%N |&| %P)) @> [
        double_neg |> L
        double_neg |> R
        trade_body |> LR |> LR' |> L'
        distrib_not_and N P |> R
        ident_implies_not_or N <@ not %P@> |> LR |> LR' |> L'
    ]

    /// exists x (Q |&| N) P = (exists x Q (N |&| P))
    let trade_exists_and_and x N P Q = ident pred_calculus <@ exists %x (%Q |&| %N) %P = (exists %x %Q (%N |&| %P)) @> [
        trade_exists_and x <@ %Q |&| %N @> P |> L
        right_assoc_and Q N P |> LR |> L'
        trade_exists_and x Q <@ %N |&| %P @> |> Commute |> L
    ]

    /// exists x (N1 ||| N2) P = ((exists x N1 P) |&| (exists x N2 P))
    let split_range_exists' x N1 N2 P = id_ax pred_calculus <@ exists %x (%N1 ||| %N2) P = ((exists %x %N1 P) |&| (exists %x %N2 P)) @>
    
    /// ((exists x N P) ||| (exists x N Q)) = (exists x N (P ||| Q)) 
    let collect_exists_or' x N P Q = id_ax pred_calculus <@ ((exists %x %N %P) ||| (exists %x %N %Q)) = (exists %x %N (%P ||| %Q)) @>

    /// exists x N (P ||| Q) = ((exists x N P) ||| (exists x N Q))   
    let distrib_exists_or' x N P Q = collect_exists_or' x N P Q |> Commute

    /// P |&| exists x N Q = (exists x N (P |&| Q)) 
    let distrib_and_exists_and x N P Q = ident pred_calculus <@ %P |&| exists %x %N %Q = (exists %x %N (%P |&| %Q)) @> [
        do fail_if_occurs_free x P  
        double_neg |> R |> L'
        double_negation P |> Commute |> L |> L'
        collect_not_or <@ not %P @> <@ forall %x %N (not %Q) @> |> L
        distrib_or_forall' x N <@ not %P @> <@ not %Q @> |> LR |> LR' |> L'
        double_neg |> R
        distrib_not_and P Q |> LR |> LR' |> R' 
    ]

    /// exists x N P = (P |&| (exists x true N))
    let distrib_and_exists x N P = ident pred_calculus <@ exists %x %N %P = (%P |&| (exists' %x %N)) @> [
        do fail_if_occurs_free x P
        distrib_and_exists_and x <@ true @> P N |> R
        trade_exists_and x N P |> L
        commute_and N P |> LR |> L'
    ]

    /// exists x true N ==> ((exists x N (P ||| Q)) = (P ||| exists x N Q))
    let trade_exists_or x N P Q = theorem pred_calculus <@ exists' %x %N ==> ((exists %x %N (%P ||| %Q)) = (%P ||| exists %x %N %Q)) @> [
        do fail_if_occurs_free x P
        distrib_and_exists x N <@ %P ||| %Q @> |> L |> R'
        distrib_and_or <@ exists' %x %N @> P Q |> CommuteL |> L |> R'
        distrib_and_exists x N Q |> Commute |> CommuteL |> L |> R'
        axiom prop_calculus <@ exists' %x %N ==> exists' %x %N @> |> Deduce |> L |> R'
        ident_and P |> CommuteL |> L |> R'
        def_true <@ %P ||| (exists %x %N %Q) @> |> Commute |> R
    ]

    /// exists x N (false) = false
    let ident_exists_false x N = ident pred_calculus <@ exists %x %N (false) = false @> [
        distrib_and_exists x N <@ false @> |> L
        commute |> L
        zero_and <@ exists' %x %N @> |> L
    ]

    /// exists x N P ==> (exists x (Q ||| N) P)
    let weaken_exists_range x N P Q = theorem pred_calculus <@ exists %x %N %P ==> (exists %x (%Q ||| %N) %P) @> [
        split_range_exists |> R
        commute |> R
        weaken_or <@ exists %x %N %P @> <@ exists %x %Q %P @> |> Lemma   
    ]

    /// exists x N P ==> (exists x N (P ||| Q))
    let weaken_exists_body x N P Q = theorem pred_calculus <@ exists %x %N %P ==> (exists %x %N (%P ||| %Q)) @> [
        distrib_exists_or' x N P Q |> R 
        weaken_or <@ exists %x %N %P @> <@ exists %x %N %Q @> |> Lemma
    ]

    (*
    /// exists' x (forall' y P) ==> (forall' y (exists' x P))
    let exists_forall_interchange' x y P= theorem pred_calculus <@ exists' %x (forall' %y %P) ==> (forall' %y (exists' %x %P)) @> [        
        def_implies |> LR
        distrib_or_forall |> L
        collect_exists_or' x <@ true @> <@ forall' %y %P @> P |> QB |> L'
        inst' y P |> L |> QB' |> QB' |> L'
        idemp |> QB |> QB' |> L'
    ]
    *)
    /// exists x N P ==> Q ==> (N |&| P ==> Q)
    (*
    let ident_exists_implies x N P Q = ident pred_calculus <@ exists %x %N %P ==> %Q = (%N |&| %P ==> %Q) @> [
        trade_body |> L |> L'
        ident_implies_not_or <@ (exists' %x (%N |&| %P)) @> Q |> L
        ident_not_exists_forall_not' x <@ %N |&| %P@> |> L |> L'
        inst' x <@ not (%N |&| %P)@> |> L |> L'
        ident_implies_not_or <@ %N |&| %P@> Q |> Commute |> L   
    ]
    *)