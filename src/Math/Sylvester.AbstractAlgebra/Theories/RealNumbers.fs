namespace Sylvester

open System
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Patterns
open Descriptions

type RealExpr = Expr<real>

/// Theory of the field of real numbers with the least.
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
         | Divide(Double l, Double r) -> <@@ l / r @@>
         | expr -> traverse expr _reduce_constants

    let rec _distrib_divide_add =
        function
        | Divide(a1, Add(a2, a3)) -> call_add (call_mul a1 a2)  (call_mul a1 a3) 
        | expr -> traverse expr _distrib_divide_add

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
    let reduce = Admit("Reduce real number constants in (expression)", _reduce_constants)

    /// Expression is distributive.
    let distrib = Admit("(expression) is distributive", _distrib_divide_add)

    /// Collect multiplication terms distributed over addition.
    let collect = Admit("Collect multiplication terms distributed over addition in (expression)", _collect)

    /// Zero is the identity of the addition operation.
    let zero_ident = Admit("Zero is the identity of the addition operation in (expression)", _collect)

    (* Theory *)
    type RealNumbers() = inherit Theory(real_numbers_axioms, [
        Integers.reduce
        Integers.right_assoc_add
        Integers.right_assoc_mul
        Integers.left_assoc_add
        Integers.left_assoc_mul
        Integers.commute_add
        Integers.commute_mul
        Integers.distrib_mul_add
        Integers.distrib_mul_sub
        Integers.collect_mul_add
        Integers.collect_mul_sub
        Integers.ident_add
        Integers.ident_mul
        reduce
        distrib
        collect
        zero_ident
    ])
    
    let real_numbers = RealNumbers()

    (* Functions *)

    let sup (s:IOrderedSet<real>) = formula<real>

    let inf (s:IOrderedSet<real>) = formula<real>

    (* Predicates *)

    let bounded_above = pred<IOrderedSet<real>>
    
    let bounded_below = pred<IOrderedSet<real>>

    let bounded = pred<IOrderedSet<real>>


    (* Definitions *)

    let lub (S:Expr<IOrderedSet<real>>) (E:Expr<IOrderedSet<real>>) = def real_numbers <@ forall %E (%E |<| %S) (not_empty %E |&| bounded_above %E ==> (sup %E |?| %S)) @>