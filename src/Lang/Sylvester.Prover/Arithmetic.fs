namespace Sylvester.Prover

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

open Sylvester

open FormulaPatterns

[<ReflectedDefinition>]
module Arithmetic =
    let integer_axioms = 
        function
        | Equal x  
        | Assoc x 
        | Commute x -> true 
        | _ -> false

    let rec _reduce_constants  =
        function
        | Add(Int32 l, Int32 r) -> Expr.Value(l + r)
        | Subtract(Int32 l, Int32 r) -> Expr.Value(l - r)
        | Multiply(Int32 l, Int32 r) -> Expr.Value(l * r)
        | expr -> traverse expr _reduce_constants

    let reduce_constants (a, b) = _reduce_constants a, _reduce_constants b
    
    let Arithmetic = ProofSystem(integer_axioms, [reduce_constants])
