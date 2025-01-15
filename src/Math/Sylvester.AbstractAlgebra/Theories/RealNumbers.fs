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
         | expr -> traverse expr (Integers._reduce_constants >> _reduce_constants)

    let rec _distrib_div_add =
        function
        | Divide(a1, Add(a2, a3)) -> call_add (call_div a1 a2)  (call_div a1 a3) 
        | expr -> traverse expr _distrib_div_add

    let rec _collect_div_add =
        function
        | Add(Divide(a1, a2), Divide(a1', a3)) when sequal a1 a1' -> call_div a1 (call_add a2 a3) 
        | expr -> traverse expr _collect_div_add

    
    let rec _distrib_div_sub =
        function
        | Divide(a1, Subtract(a2, a3)) -> call_sub (call_div a1 a2)  (call_div a1 a3) 
        | expr -> traverse expr _distrib_div_sub

    let rec _collect_div_sub =
        function
        | Subtract(Divide(a1, a2), Divide(a1', a3)) when sequal a1 a1' -> call_div a1 (call_sub a2 a3) 
        | expr -> traverse expr _collect_div_sub

    let rec _ident_div =
        function
        | Divide(x, Double 1.) -> x
        | expr -> traverse expr _ident_div

    let right_assoc_add = Integers.right_assoc_add
    
    let right_assoc_mul = Integers.right_assoc_mul
    
    let left_assoc_add = Integers.left_assoc_add
    
    let left_assoc_mul = Integers.left_assoc_mul
    
    let commute_add = Integers.commute_add
    
    let commute_mul = Integers.commute_mul
    
    let distrib_mul_add = Integers.distrib_mul_add
    
    let distrib_mul_sub = Integers.distrib_mul_sub
    
    let collect_mul_add = Integers.collect_mul_add
    
    let collect_mul_sub = Integers.collect_mul_sub
    
    let ident_add = Integers.ident_add
    
    let ident_mul = Integers.ident_mul
    
    /// Reduce equal constants in expression. 
    let reduce = Admit("Reduce real number constants in (expression)", _reduce_constants)

    /// Division is distributive over addition.
    let distrib_div_add = Admit("(expression) is distributive", _distrib_div_add)

    /// Collect multiplication terms distributed over addition.
    let collect_div_add = Admit("Collect multiplication terms distributed over addition in (expression)", _collect_div_add)

    /// One is the identity of the addition operation.
    let ident_div = Admit("1 is the identity of the division operation in (expression)", _ident_div)
    
    (* Theory *)
    type RealNumbers() = inherit Theory(real_numbers_axioms, [
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
        distrib_div_add
        collect_div_add
        ident_div
    ])
    
    let real_numbers = RealNumbers()

    
    (* Functions *)

    let supremum (s:ITotalOrder<real>) = formula<real>

    let infimum (s:ITotalOrder<real>) = formula<real>

    (* Predicates *)

    let bounded_above = pred<ITotalOrder<real>>
    
    let bounded_below = pred<ITotalOrder<real>>

    let bounded = pred<ITotalOrder<real>>


    (* Definitions *)

    let lub (S:Expr<ITotalOrder<real>>) (E:Expr<ITotalOrder<real>>) = def real_numbers <| prop <@ forall %E ((%E).Set |<| set_of %S) (not_empty %E |&| bounded_above %E ===> (supremum %E |?| set_of %S)) @>