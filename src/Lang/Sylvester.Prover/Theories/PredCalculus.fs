namespace Sylvester

open PropCalculus

/// Predicate calculus using the axioms and rules of S.
module PredCalculus =
    let pred_calculus = Theory.S

    (* Expression functions for admissible rules *)
    
    let _empty_range = EquationalLogic._empty_range

    let _trading = EquationalLogic._trading

    let _distrib_forall = EquationalLogic._distrib_forall

    (* Admissible rules *)
    
    let empty_range = Theory.S.Rules.[20] 

    let trading = Theory.S.Rules.[21]

    let distrib_forall = Theory.S.Rules.[22]

    (* Theorems *)

    let trading_forall x N P = id_ax pred_calculus <@ forall %x %N %P = (forall %x true %N ==> %P) @>
        
    let trading_foralll_and_implies x Q N P = ident pred_calculus <@ forall x (%Q |&| %N) %P = (forall x %Q (%N ==> %P)) @> [
        trading_forall x <@ %Q |&| %N @> P |> L
        shunt' Q N P |> L
        trading_forall x Q  <@ %N==> %P @> |> Commute |> L
    ]

    let distrib_forall' x N P Q = id_ax pred_calculus <@ %P ||| forall %x %N %Q = (forall %x %N (%P ||| %Q)) @>