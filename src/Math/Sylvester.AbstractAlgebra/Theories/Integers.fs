namespace Sylvester

open System
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Patterns
open Descriptions

/// Theory of operations on an integral domain of integers with binary operations (+) and (*) and (*) distributes over (+), 
/// identities 0 and 1, and unary inverse operation (-), where c <> 0  ==> (c * a = c * b = (a = b)). 
/// < is defined by a < b = (b - a) |?| Zpos  
module Integers =      
    (* Symbols *)
    do Symbols.BulitIn.Add(src <@ (*) @>, "\u22C5")
    
    let desc = axiom_desc "Integers"
    
    (* Patterns *)
    
    let (|Zpos|_|) =
        function
        | ValueWithName(z, t, n) when t = typeof<OrderedRing<int>> && (z :?> OrderedRing<int>) = Ring.Zpos -> Some()
        | _ -> None

    (* Axioms *)

    let (|DefLessThan|_|) =
        function
        | Equals(LessThan(l,r), SetTheory.ElementOf(Subtract(r', l'), Zpos::[])) when sequal2 l r l' r' -> pattern_desc' "Definition of <" |> Some
        | _ -> None

    let integer_axioms =
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
        | Exists(_, a::[], Bool true, (Equals(Add(Var _, Var a'), Int32 0))) when vequal a a' -> Some (axiom_desc "Integers" (pattern_desc' "Additive Inverse")) 
        | DefLessThan x -> Some (desc x)
        | _ -> None

    (* Expression functions for derived rules *)

    let rec _reduce_constants  =
        function
        | Add(UInt16 l, UInt16 r) -> <@@ l + r @@>
        | Multiply(UInt16 l, UInt16 r) -> <@@ l * r @@>        
        | Subtract(UInt16 l, UInt16 r) -> <@@ l - r @@>

        | Add(Int16 l, Int16 r) -> <@@ l + r @@>
        | Multiply(Int16 l, Int16 r) -> <@@ l * r @@>        
        | Subtract(Int16 l, Int16 r) -> <@@ l - r @@>

        | Add(UInt32 l, UInt32 r) -> <@@ l + r @@>
        | Multiply(UInt32 l, UInt32 r) -> <@@ l * r @@>        
        | Subtract(UInt32 l, UInt32 r) -> <@@ l - r @@>

        | Add(Int32 l, Int32 r) -> <@@ l + r @@>
        | Multiply(Int32 l, Int32 r) -> <@@ l * r @@>        
        | Subtract(Int32 l, Int32 r) -> <@@ l - r @@>
        
        | Add(UInt64 l, UInt64 r) -> <@@ l + r @@>
        | Multiply(UInt64 l, UInt64 r) -> <@@ l * r @@>        
        | Subtract(UInt64 l, UInt64 r) -> <@@ l - r @@>

        | Add(Int64 l, Int64 r) -> <@@ l + r @@>
        | Multiply(Int64 l, Int64 r) -> <@@ l * r @@>        
        | Subtract(Int64 l, Int64 r) -> <@@ l - r @@>

        | Add(Rational l, Rational r) -> <@@ l + r @@>
        | Multiply(Rational l, Rational r) -> <@@ l * r @@>        
        | Subtract(Rational l, Rational r) -> <@@ l - r @@>

        | Add(Single l, Single r) -> <@@ l + r @@>
        | Multiply(Single l, Single r) -> <@@ l * r @@>        
        | Subtract(Single l, Single r) -> <@@ l - r @@>

        | Add(Double l, Double r) -> <@@ l + r @@>
        | Multiply(Double l, Double r) -> <@@ l * r @@>        
        | Subtract(Double l, Double r) -> <@@ l - r @@>
        
        | Add(Decimal l, Decimal r) -> <@@ l + r @@>
        | Multiply(Decimal l, Decimal r) -> <@@ l * r @@>        
        | Subtract(Decimal l, Decimal r) -> <@@ l - r @@>

        | expr -> traverse expr _reduce_constants

    let rec _right_assoc_add =
        function
        | Add(Add(a1, a2), a3) -> call_add a1 (call_add a2 a3)
        | Multiply(Multiply(a1, a2), a3) -> call_mul a1 (call_add a2 a3)
        | expr -> traverse expr _right_assoc_add

    let rec _right_assoc_mul =
        function
        | Multiply(Multiply(a1, a2), a3) -> call_mul a1 (call_add a2 a3)
        | expr -> traverse expr _right_assoc_mul

    let rec _left_assoc_add =
        function
        | Add(a1, Add(a2, a3)) -> call_add (call_add a1 a2) a3
        | expr -> traverse expr _left_assoc_add

    let rec _left_assoc_mul =
        function
        | Multiply(a1, Multiply(a2, a3)) -> call_mul (call_mul a1 a2) a3
        | expr -> traverse expr _left_assoc_mul

    let rec _commute_add =
        function
        | Add(a1, a2) -> call_add a2 a1
        | expr -> traverse expr _commute_add

    let rec _commute_mul =
        function
        | Multiply(a1, a2) -> call_mul a2 a1
        | expr -> traverse expr _commute_mul

    let rec _distrib_mul_add =
        function
        | Multiply(a1, Add(a2, a3)) -> call_add (call_mul a1 a2)  (call_mul a1 a3) 
        | expr -> traverse expr _distrib_mul_add

    let rec _distrib_mul_sub =
        function 
        | Multiply(a1, Subtract(a2, a3)) -> call_sub (call_mul a1 a2) (call_mul a1 a3)
        | expr -> traverse expr _distrib_mul_sub

    let rec _collect_mul_add =
        function
        | Add(Multiply(a1, a2), Multiply(a1', a3)) when sequal a1 a1' -> call_mul a1 (call_add a2 a3) 
        | Subtract(Multiply(a1, a2), Multiply(a1', a3)) when sequal a1 a1' -> call_mul a1 (call_sub a2 a3) 
        | expr -> traverse expr _collect_mul_add

    let rec _collect_mul_sub =
         function
         | Subtract(Multiply(a1, a2), Multiply(a1', a3)) when sequal a1 a1' -> call_mul a1 (call_sub a2 a3) 
         | expr -> traverse expr _collect_mul_sub

    let rec _ident_add =
        function
        | Add(x, Int32 0) -> x
        | expr -> traverse expr _ident_add

    let rec _ident_mul =
        function
        | Multiply(x, Int32 1) -> x
        | expr -> traverse expr _ident_mul

    (* Admitted rules *)

    /// Reduce equal constants in expression. 
    let reduce = Admit("Reduce integer constants in (expression)", _reduce_constants)

    /// Addition is right associative.
    let right_assoc_add = Admit("Addition operations in (expression) are right-associative", _right_assoc_add)

    /// Multiplication is right associative.
    let right_assoc_mul = Admit("Multplications operations in (expression) is right-associative", _right_assoc_mul)

    /// Addition is left associative.
    let left_assoc_add = Admit("Addition operations in (expression) are left-associative", _left_assoc_add)

    /// Multiplication is left associative.
    let left_assoc_mul = Admit("Addition operations in (expression) are left-associative", _left_assoc_mul)

    /// Addition is comutative.
    let commute_add = Admit("Addition operation in (expression) is commutative", _commute_add)

    /// Multiplication is comutative.
    let commute_mul = Admit("Multiplication operation in (expression) is commutative", _commute_mul)

    /// Multiplication is distributive over addition.
    let distrib_mul_add = Admit("Multipication distributes over addition in (expression)", _distrib_mul_add)

    /// Multiplication is distributive over subtraction.
    let distrib_mul_sub = Admit("Multipication distributes over addition in (expression)", _distrib_mul_sub)

    /// Collect multiplication terms distributed over addition.
    let collect_mul_add = Admit("Collect multiplication terms distributed over addition in (expression)", _collect_mul_add)

    /// Collect multiplication terms distributed over subtraction.
    let collect_mul_sub = Admit("Collect multiplication terms distributed over addition in (expression)", _collect_mul_sub)

    /// Zero is the identity of the addition operation.
    let ident_add = Admit("0 is the identity of the addition operation in (expression)", _ident_add)

    /// One is the identity of the multiplication operation.
    let ident_mul = Admit("1 is the identity of the multiplication operation in (expression)", _ident_mul)

    type Integers() = inherit Theory(integer_axioms, [
        reduce
        right_assoc_add
        right_assoc_mul
        left_assoc_add
        left_assoc_mul
        commute_add
        commute_mul
        distrib_mul_add
        distrib_mul_sub
        collect_mul_add
        collect_mul_sub
        ident_add
        ident_mul
    ])
    
    let integers = Integers()