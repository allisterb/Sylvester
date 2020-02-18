namespace Sylvester

open System

open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.ExprShape

open FormulaPatterns

type Axioms = (Expr * Expr->Expr option)
type Rules = (Expr->Expr) list 
type ProofCalculus =  ProofCalculus of Axioms * Rules

module Proof =   
    let integerAxioms = 
        function
        | Assoc x -> Some x
        | _ -> None

    let rec reduce_constants  =
        function
        | Add(Int32 l, Int32 r) -> Expr.Value(l + r)
        | Subtract(Int32 l, Int32 r) -> Expr.Value(l - r)
        | Multiply(Int32 l, Int32 r) -> Expr.Value(l * r)
        | expr -> traverse expr reduce_constants

    let rec right_assoc =
        function
        | Add(Add(a1, a2), a3) -> <@@ %%a1 + (%%a2 + %%a3)@@>
        | expr -> traverse expr right_assoc

    let rec left_assoc =
        function
        | Add(a1, Add(a2, a3)) -> <@@ (%%a1 + %%a2) + %%a3@@>
        | expr -> traverse expr left_assoc

    let IntegerCalculus = ProofCalculus(integerAxioms, [reduce_constants; right_assoc; left_assoc])
