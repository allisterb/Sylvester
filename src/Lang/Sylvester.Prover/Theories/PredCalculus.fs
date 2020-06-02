namespace Sylvester

open PropCalculus

/// Predicate calculus using the axioms and rules of S.
module PredCalculus =
    let pred_calculus = Theory.S

    (* Expression functions for admissible rules *)
    
    let _empty_range = EquationalLogic._empty_range

    let _trade = EquationalLogic._trade

    let _distrib_or_forall = EquationalLogic._distrib_or_forall

    (* Admissible rules *)
    
    let empty_range = Theory.S.Rules.[20] 

    let trading = Theory.S.Rules.[21]

    let collect_forall_and = Theory.S.Rules.[22]

    let collect_exists_or = Theory.S.Rules.[23]

    let distrib_or_forall = Theory.S.Rules.[24]

    (* Theorems *)

    let trade_forall x N P = id_ax pred_calculus <@ forall %x %N %P = (forall %x true (%N ==> %P)) @>
        
    let trade_forall_and_implies x Q N P = ident pred_calculus <@ forall x (%Q |&| %N) %P = (forall x %Q (%N ==> %P)) @> [
        trade_forall x <@ %Q |&| %N @> P |> L
        shunt' Q N P |> L
        trade_forall x Q  <@ %N==> %P @> |> Commute |> L
    ]

    let trade_forall_or_not x N P = ident pred_calculus <@ forall %x %N %P = (%P ||| forall' %x (not %N))  @> [
        distrib_or_forall |> R
        commute_or P <@ not %N @> |> R
        ident_implies_not_or N P |> Commute |> R 
    ]

    let distrib_or_forall' x N P Q = id_ax pred_calculus <@ %P ||| forall %x %N %Q = (forall %x %N (%P ||| %Q)) @>

    let collect_forall_and' x N P Q = id_ax pred_calculus <@ ((forall %x %N %P) |&| (forall %x %N %Q)) = (forall %x %N (%P |&| %Q))@>

    let distrib_forall_and' x N P Q = collect_forall_and' x N P Q |> Commute

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

    let ident_forall_true x N = ident pred_calculus <@ (forall %x %N true) = true @> [
        trade_forall_or_not x N <@ true @> |> L
        commute |> L
        zero_or <@ forall' x (not %N ) @> |> L 
    ]

    let ident_forall_eq x N P Q = ident pred_calculus <@ forall x %N (%P = %Q) ==> (forall %x %N %P  = (forall %x %N %Q)) @> [
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