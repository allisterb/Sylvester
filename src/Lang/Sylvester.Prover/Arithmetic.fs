namespace Sylvester.Prover

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

open Sylvester

open FormulaPatterns

module Arithmetic =

    let integer_axioms = 
        function
        | Assoc x -> Some x
        | Commute x -> Some x
        | _ -> None

    let rec _reduce_constants  =
        function
        | Add(Int32 l, Int32 r) -> Expr.Value(l + r)
        | Subtract(Int32 l, Int32 r) -> Expr.Value(l - r)
        | Multiply(Int32 l, Int32 r) -> Expr.Value(l * r)
        | expr -> traverse expr _reduce_constants

    let reduce_constants (a, b) = _reduce_constants a, _reduce_constants b
    
    let rec right_assoc =
        function
        | Add(Add(a1, a2), a3) -> <@@ %%a1 + (%%a2 + %%a3)@@>
        | expr -> traverse expr right_assoc

    let rec left_assoc =
        function
        | Add(a1, Add(a2, a3)) -> <@@ (%%a1 + %%a2) + %%a3@@>
        | expr -> traverse expr left_assoc

    let left_assoc_a (a, b) = left_assoc a, b
    let left_assoc_b (a, b) =  a, left_assoc b

    let Arithmetic = ProofCalculus(integer_axioms, [reduce_constants; left_assoc_a])
