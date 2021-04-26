namespace Sylvester

open System
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Patterns
open Descriptions

/// Theory of algebraic operations on an integral domain of integers with binary operations (+) and (*) and (*) distributes over (+), 
/// identities 0 and 1, and unary inverse operation (-), where c <> 0  ==> (c * a = c * b = (a = b)).
module IntegerAlgebra =      
    (* Symbols *)
    do Symbols.BulitIn.Add(src <@ (*) @>, "\u22C5")
    
    let desc = axiom_desc "Integer Algebra"
    
    (* Axioms *)
    let integer_algebra_axioms =
        function                    
        | Assoc <@(=)@> (<@ (+) @> :Expr<int->int->int>) x
        | Assoc <@(=)@> (<@ (*) @> :Expr<int->int->int>) x
        | Commute <@(=)@> (<@ (+) @> :Expr<int->int->int>) x
        | Commute <@(=)@> (<@ (*) @> :Expr<int->int->int>) x
        | Identity <@(=)@> (<@ (+) @> :Expr<int->int->int>) <@ 0 @> x 
        | Identity <@(=)@> (<@ (*) @> :Expr<int->int->int>) <@ 1 @> x
        | Inverse <@(=)@> (<@ (+) @> :Expr<int->int->int>) <@ (~-) @> <@ 0 @> x
        | Distrib <@(=)@> (<@ (*) @> :Expr<int->int->int>) (<@ (+) @> :Expr<int->int->int>) x  
        | LeftCancelNonZero (<@ (+) @> :Expr<int->int->int>) <@ 0 @> x
        | BinaryOpDefR <@(=)@> <@ (-) @> (<@ (+) @> :Expr<int->int->int>) <@ (~-) @> x  -> Some (desc x)
        | Exists(_, a::[], Bool true, (Equals(Add(Var _, Var a'), Int32 0))) when vequal a a' -> Some (axiom_desc "Integer Algebra" (pattern_desc' "Additive inverse")) 
        | _ -> None

    let rec _reduce_constants  =
        function
        | Add(UInt16 l, UInt16 r) -> <@@ l + r @@>  
        | Add(UInt32 l, UInt32 r) -> <@@ l + r @@>
        | Add(UInt64 l, UInt64 r) -> <@@ l + r @@>
        | Add(Int16 l, Int16 r) -> <@@ l + r @@>  
        | Add(Int32 l, Int32 r) -> <@@ l + r @@>
        | Add(Int64 l, Int64 r) -> <@@ l + r @@>
        | Add(Decimal l, Decimal r) -> <@@ l + r @@>  
        | Add(Decimal l, Decimal r) -> <@@ l + r @@>
        | Add(Decimal l, Decimal r) -> <@@ l + r @@>
        | Add(Double l, Double r) -> <@@ l + r @@>  
        | Add(Double l, Double r) -> <@@ l + r @@>
        | Add(Double l, Double r) -> <@@ l + r @@>
        | Add(Decimal l, Decimal r) -> <@@ l + r @@>  
        | Add(Decimal l, Decimal r) -> <@@ l + r @@>
        | Add(Decimal l, Decimal r) -> <@@ l + r @@>

        | Multiply(UInt16 l, UInt16 r) -> <@@ l * r @@>  
        | Multiply(UInt32 l, UInt32 r) -> <@@ l * r @@>
        | Multiply(UInt64 l, UInt64 r) -> <@@ l * r @@>
        | Multiply(Int16 l, Int16 r) -> <@@ l * r @@>  
        | Multiply(Int32 l, Int32 r) -> <@@ l * r @@>
        | Multiply(Int64 l, Int64 r) -> <@@ l * r @@>
        | Multiply(Decimal l, Decimal r) -> <@@ l * r @@>  
        | Multiply(Decimal l, Decimal r) -> <@@ l * r @@>
        | Multiply(Decimal l, Decimal r) -> <@@ l * r @@>
        | Multiply(Double l, Double r) -> <@@ l * r @@>  
        | Multiply(Double l, Double r) -> <@@ l * r @@>
        | Multiply(Double l, Double r) -> <@@ l * r @@>
        | Multiply(Decimal l, Decimal r) -> <@@ l * r @@>  
        | Multiply(Decimal l, Decimal r) -> <@@ l * r @@>
        | Multiply(Decimal l, Decimal r) -> <@@ l * r @@>

        | Subtract(UInt16 l, UInt16 r) -> <@@ l - r @@>  
        | Subtract(UInt32 l, UInt32 r) -> <@@ l - r @@>
        | Subtract(UInt64 l, UInt64 r) -> <@@ l - r @@>
        | Subtract(Int16 l, Int16 r) -> <@@ l - r @@>  
        | Subtract(Int32 l, Int32 r) -> <@@ l - r @@>
        | Subtract(Int64 l, Int64 r) -> <@@ l - r @@>
        | Subtract(Decimal l, Decimal r) -> <@@ l - r @@>  
        | Subtract(Decimal l, Decimal r) -> <@@ l - r @@>
        | Subtract(Decimal l, Decimal r) -> <@@ l - r @@>
        | Subtract(Double l, Double r) -> <@@ l - r @@>  
        | Subtract(Double l, Double r) -> <@@ l - r @@>
        | Subtract(Double l, Double r) -> <@@ l - r @@>
        | Subtract(Decimal l, Decimal r) -> <@@ l - r @@>  
        | Subtract(Decimal l, Decimal r) -> <@@ l - r @@>
        | Subtract(Decimal l, Decimal r) -> <@@ l - r @@>

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
        | Add(x, Int32 0) -> x
        | Multiply(x, Int32 1) -> x
        | expr -> traverse expr _ident

    /// Reduce equal constants in expression. 
    let reduce = Admit("Reduce integer constants in (expression)", _reduce_constants)

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

    type IntegerAlgebra() = inherit Theory(integer_algebra_axioms, [
        reduce
        right_assoc
        left_assoc
        commute
        distrib
        collect
        zero_ident
    ])
    
    let integer_algebra = IntegerAlgebra()