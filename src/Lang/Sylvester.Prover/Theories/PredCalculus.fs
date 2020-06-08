namespace Sylvester

open PropCalculus

/// Predicate calculus using the axioms and rules of S.
module PredCalculus =
    let pred_calculus = Theory.S

    (* Expression functions for admissible rules *)
    
    let _empty_range = EquationalLogic._empty_range

    let _trade_body = EquationalLogic._trade_body

    let _distrib_or_forall = EquationalLogic._distrib_or_forall

    (* Admissible rules *)
    
    let empty_range = Theory.S.Rules.[19] 

    let trade_body = Theory.S.Rules.[20]

    let collect_forall_and = Theory.S.Rules.[21]

    let collect_exists_or = Theory.S.Rules.[22]

    let distrib_or_forall = Theory.S.Rules.[23]

    let split_range_forall = Theory.S.Rules.[24]

    (* Theorems *)

    /// forall x N P = (forall x true (N ==> P))
    let trade_forall_implies x N P = id_ax pred_calculus <@ forall %x %N %P = (forall %x true (%N ==> %P)) @>
        
    /// forall x (Q |&| N) P = (forall x Q (N ==> P))
    let trade_forall_and_implies x Q N P = ident pred_calculus <@ forall x (%Q |&| %N) %P = (forall x %Q (%N ==> %P)) @> [
        trade_forall_implies x <@ %Q |&| %N @> P |> L
        shunt' Q N P |> L
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
        let lemma1 = proof pred_calculus <@ not (forall' %x (not %N)) ==> (forall' %x (not %N) = false) @> [
            distrib_implies_eq_and <@ (not (forall' %x (not %N))) @> <@ forall' %x (not %N) @> <@ false @> |> LR
            contr <@ forall' %x (not %N) @> |> CommuteL |> L
            zero_and <@ not (forall' %x (not %N)) @> |> R   
        ]
        distrib_forall_and' x N P Q |> R
        trade_forall_or_not x N P |> R
        deduce' lemma1
        ident_or P |> R
        def_true <@ %P |&| forall %x %N %Q @> |> Commute |> R
    ]

    /// (forall x N true) = true
    let ident_forall_true x N = ident pred_calculus <@ (forall %x %N true) = true @> [
        trade_forall_or_not x N <@ true @> |> L
        commute |> L
        zero_or <@ forall' x (not %N ) @> |> L 
    ]

    /// forall x N (P = Q) ==> (forall x N P  = (forall x N Q))
    let ident_forall_eq x N P Q = theorem pred_calculus <@ forall %x %N (%P = %Q) ==> (forall %x %N %P  = (forall %x %N %Q)) @> [
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

    /// forall %x (%R1 ||| %R2) %P ==> (forall %x %R1 %P)
    let strengthen_forall_range_or x R1 R2 P = theorem pred_calculus <@ (forall %x (%R1 ||| %R2) %P) ==> (forall %x %R1 %P) @> [
        split_range_forall |> L
        strenghten_and <@ forall %x %R1 %P @> <@ forall %x %R1 %P @> |> Lemma
    ]

    /// forall %x %N (%P |&| %Q) ==> (forall %x %N %P)
    let strengthen_forall_body_and x N P Q = theorem pred_calculus <@ (forall %x %N (%P |&| %Q)) ==> (forall %x %N %P) @> [
        distrib_forall_and' x N P Q |> L
        strenghten_and <@ forall %x %N %P @> <@ forall %x %N %Q @> |> Lemma
    ]