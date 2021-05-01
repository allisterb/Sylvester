namespace Sylvester

open System
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Patterns
open Descriptions

/// Theory of real numbers
module RealNumbers =      
    let desc = axiom_desc "Real Numbers"
    
    (* Axioms *)
    let real_numbers_axioms =
        function                            
        | Assoc <@(=)@> (<@ (+) @> :Expr<real->real->real>) x
        | Assoc <@(=)@> (<@ (*) @> :Expr<real->real->real>) x
        | Commute <@(=)@> (<@ (+) @> :Expr<real->real->real>) x
        | Commute <@(=)@> (<@ (*) @> :Expr<real->real->real>) x
        | Identity <@(=)@> (<@ (+) @> :Expr<real->real->real>) <@ 0. @> x 
        | Identity <@(=)@> (<@ (*) @> :Expr<real->real->real>) <@ 1. @> x
        | Inverse <@(=)@> (<@ (+) @> :Expr<real->real->real>) <@ (~-) @> <@ 0. @> x
        | Inverse <@(=)@> (<@ (/) @> :Expr<real->real->real>) <@ inv @> <@ 1. @> x
        | Distrib <@(=)@> (<@ (*) @> :Expr<real->real->real>) (<@ (+) @> :Expr<real->real->real>) x  
        | LeftCancelNonZero (<@ (+) @> :Expr<real->real->real>) <@ 0. @> x
        | LeftCancelNonZero (<@ (/) @> :Expr<real->real->real>) <@ 1. @> x
        | BinaryOpDefR <@(=)@> <@ (-) @> (<@ (+) @> :Expr<real->real->real>) <@ (~-) @> x  -> Some (desc x)
        | BinaryOpDefR <@(=)@> <@ (/) @> (<@ (*) @> :Expr<real->real->real>) <@ inv @> x  -> Some (desc x)
        | Exists(_, a::[], Bool true, (Equals(Add(Var _, Var a'), Double 0.))) when vequal a a' -> Some (desc (pattern_desc' "Additive Inverse"))
        | Exists(_, a::[], Bool true, (Equals(Multiply(Var _, Var a'), Double 1.))) when vequal a a' -> Some (desc (pattern_desc' "Multiplicative Inverse"))
        | _ -> None
    
    let rec _reduce_constants  =
         function
         | Add(Double l,  Double r) -> <@@ l + r @@>
         | Multiply(Double l, Double r) -> <@@ l * r @@>        
         | Subtract(Double l, Double r) -> <@@ l - r @@>
         | Divide(Double l, Double r) -> <@@ l / r @@>
         | expr -> traverse expr _reduce_constants

    let rec _right_assoc =
        function
        | Add(Add(a1, a2), a3) -> call_add a1 (call_add a2 a3)
        | Multiply(Multiply(a1, a2), a3) -> call_mul a1 (call_add a2 a3)
        | expr -> traverse expr _right_assoc

    let rec _left_assoc =
        function
        | Add(a1, Add(a2, a3)) -> call_add (call_add a1 a2) a3
        | Multiply(a1, Multiply(a2, a3)) -> call_mul (call_mul a1 a2) a3
        | expr -> traverse expr _left_assoc

    let rec _commute =
        function
        | Add(a1, a2) -> call_add a2 a1
        | Multiply(a1, a2) -> call_mul a2 a1
        | expr -> traverse expr _commute

    let rec _distrib =
        function
        | Multiply(a1, Add(a2, a3)) -> call_add (call_mul a1 a2)  (call_mul a1 a3) 
        | Multiply(a1, Subtract(a2, a3)) -> call_sub (call_mul a1 a2) (call_mul a1 a3) 
        | expr -> traverse expr _distrib

    let rec _collect =
        function
        | Add(Multiply(a1, a2), Multiply(a1', a3)) when sequal a1 a1' -> call_mul a1 (call_add a2 a3) 
        | Subtract(Multiply(a1, a2), Multiply(a1', a3)) when sequal a1 a1' -> call_mul a1 (call_sub a2 a3) 
        | expr -> traverse expr _collect

    let rec _ident =
        function
        | Add(x, Double 0.) -> x
        | Multiply(x, Double 1.) -> x
        | expr -> traverse expr _ident

    /// Reduce equal constants in expression. 
    let reduce = Admit("Reduce real constants in (expression)", _reduce_constants)

    /// Expression is right associative.
    let right_assoc = Admit("(expression) is right-associative", _right_assoc)

    /// Expression is left associative.
    let left_assoc = Admit("(expression) is left-associative", _left_assoc)

    /// Expression is comutative.
    let commute = Admit("(expression) is commutative", _commute)

    /// Expression is distributive.
    let distrib = Admit("(expression) is distributive", _distrib)

    /// Collect multiplication terms distributed over addition.
    let collect = Admit("Collect multiplication terms distributed over addition in (expression)", _collect)

    /// Zero is the identity of the addition operation.
    let zero_ident = Admit("Zero is the identity of the addition operation in (expression)", _collect)

    type RealNumbers() = inherit Theory(real_numbers_axioms, [
        reduce
        right_assoc
        left_assoc
        commute
        distrib
        collect
        zero_ident
    ])
    
    let real_numbers = RealNumbers()

    (* Predicates *)
