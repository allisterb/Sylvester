namespace Sylvester

open System

open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.ExprShape


type ProofSystem =  ProofSystem of (Expr->Expr->Expr option) * (Expr->Expr list)

module IntegerAxioms =
    open ArithmeticPatterns
    
    let integer_axioms = 
        function
        | Assoc x -> Some x
        | _ -> None
    
    let axiomatic_equality (l:Formula<)
    let rec reduce_constants  =
        function
        | Add(Int32 l, Int32 r) -> Expr.Value(l + r)
        | Subtract(Int32 l, Int32 r) -> Expr.Value(l - r)
        | Multiply(Int32 l, Int32 r) -> Expr.Value(l * r)
        | Add(Add(l, Int32 lval), Int32 rval) -> let s = Expr.Value(lval + rval) in <@@ %%l + %%s @@>
        | expr -> traverse expr reduce_constants

    let rec right_assoc =
        function
        | Add(Add(a1, a2), a3) -> <@@ %%a1 + (%%a2 + %%a3)@@>
        | expr -> traverse expr right_assoc

    let rec left_assoc =
        function
        | Add(a1, Add(a2, a3)) -> <@@ (%%a1 + %%a2) + %%a3@@>
        | expr -> traverse expr left_assoc


