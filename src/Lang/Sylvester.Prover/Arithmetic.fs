namespace Sylvester.Prover

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

open Sylvester

module Arithmetic =
    open FormulaPatterns
    
    let integer_axioms = 
        function
        | Equal x  
        | Assoc x 
        | Commute x -> true 
        | _ -> false

    let rec reduce_constants  =
        function
        | Add(Int32 l, Int32 r) -> Expr.Value(l + r)
        | Subtract(Int32 l, Int32 r) -> Expr.Value(l - r)
        | Multiply(Int32 l, Int32 r) -> Expr.Value(l * r)
        | expr -> traverse expr reduce_constants

    let reduce_constants_a_b = Rule("Reduce constants in A and B.", fun (a,b) -> reduce_constants a, reduce_constants b)
    
    let rec right_assoc =
        function
        | Add(Add(a1, a2), a3) -> <@@ %%a1 + (%%a2 + %%a3)@@>
        | expr -> traverse expr right_assoc

    let rec left_assoc =
        function
        | Add(a1, Add(a2, a3)) -> <@@ (%%a1 + %%a2) + %%a3@@>
        | expr -> traverse expr left_assoc

    let left_assoc_a = Rule("A is left associative.", fun (a,b) -> left_assoc a, b)
    
    let left_assoc_b = Rule("B is left associative", fun (a, b) -> a, left_assoc b)

    let right_assoc_a = Rule("A is right associative.", fun (a, b) -> right_assoc a, b)
    
    let right_assoc_b = Rule("B is right associative.", fun (a, b) -> a, right_assoc b)

    let Arithmetic = ProofSystem(integer_axioms, [reduce_constants_a_b; left_assoc_a; left_assoc_b; right_assoc_a; right_assoc_b])
