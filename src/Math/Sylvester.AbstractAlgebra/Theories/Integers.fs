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
        | Add(Int32 l, Int32 r) -> <@@ l + r @@>
        | Multiply(Int32 l, Int32 r) -> <@@ l * r @@>        
        | Subtract(Int32 l, Int32 r) -> <@@ l - r @@>        
        | expr -> traverse expr _reduce_constants

    let rec _right_assoc_add =
        function
        | Add(Add(a1, a2), a3) -> call_add a1 (call_add a2 a3)
        | Multiply(Multiply(a1, a2), a3) -> call_mul a1 (call_add a2 a3)
        | expr -> traverse expr _right_assoc_add

    let rec _right_assoc_mul =
        function
        | Multiply(Multiply(a1, a2), a3) -> call_mul a1 (call_add a2 a3)
        | expr -> traverse expr _right_assoc_add

    let rec _left_assoc_add =
        function
        | Add(a1, Add(a2, a3)) -> call_add (call_add a1 a2) a3
        | expr -> traverse expr _left_assoc_add

    let rec _left_assoc_mul =
        function
        | Multiply(a1, Multiply(a2, a3)) -> call_mul (call_mul a1 a2) a3
        | expr -> traverse expr _left_assoc_add

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

    let rec _ident =
        function
        | Add(x, Int32 0) -> x
        | Multiply(x, Int32 1) -> x
        | expr -> traverse expr _ident

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
    let commute_mul = Admit("Multiplication operation in (expression) is commutative", _commute_add)

    /// Expression is distributive.
    //let distrib = Admit("Multipication distributes over addition (expression) is distributive", _distrib)

    /// Collect multiplication terms distributed over addition.
    //let collect = Admit("Collect multiplication terms distributed over addition in (expression)", _collect)

    /// Zero is the identity of the addition operation.
    let zero_ident = Admit("Zero is the identity of the addition operation in (expression)", _ident)

    type Integers() = inherit Theory(integer_axioms, [
        reduce
        right_assoc_add
        right_assoc_mul
        left_assoc_add
        left_assoc_mul
        commute_add
        commute_mul
        //distrib
        //collect
        zero_ident
    ])
    
    let integers = Integers()