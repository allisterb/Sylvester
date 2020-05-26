namespace Sylvester

open FSharp.Quotations

open PropCalculus

/// Predicate calculus using the axioms and rules of S.
module PredCalculus =
    let pred_calculus = Theory.S

    (* Expression functions for admissible rules *)
    
    let _empty_range = EquationalLogic._empty_range

    let _trading = EquationalLogic._trading

    (* Admissible rules *)
    
    let empty_range = Theory.S.Rules.[20] 

    let trading = Theory.S.Rules.[21]