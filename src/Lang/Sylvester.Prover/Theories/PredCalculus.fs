namespace Sylvester

open FSharp.Quotations

open PropCalculus

/// Propositional calculus using the axioms and rules of S.
module PredCalculus =
    let pred_calculus = Theory.S

    (* Expression functions for admissible rules *)
    
    let _empty_range = EquationalLogic._empty_range

    (* Admissible rules *)
    
    let empty_range = Theory.S.Rules.[20] 