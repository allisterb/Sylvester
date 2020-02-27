namespace Sylph

open System
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.ExprShape

open Sylvester
open Operators

module BooleanAlgebra =
    let rec reduce_constants  =
        function
        | Or(Bool l, Bool r) -> Expr.Value(l ||| r)        
        | Not(Bool l) -> Expr.Value(not l)        
        | And(Bool l, Bool r) -> Expr.Value(l |&| r)
        //| Implies(Bool l, Bool r) -> Expr.Value(l ==> r)
        | Equiv(Bool l, Bool r) -> Expr.Value((l = r))
        | expr -> traverse expr reduce_constants

    let rec right_assoc =
        function
        | Or(Or(a1, a2), a3) -> <@@ %%a1 ||| (%%a2 ||| %%a3) @@>
        | And(And(a1, a2), a3) -> <@@ %%a1 |&| (%%a2 |&| %%a3) @@>
        | Equiv(Equiv(a1, a2), a3) -> <@@ (%%a1 : Boolean) = ((%%a2:Boolean) = %%a3:bool) @@> 
        | expr -> traverse expr right_assoc

    let rec left_assoc =
        function
        | Or(a1, Or(a2, a3)) -> <@@ (%%a1 ||| %%a2) ||| %%a3 @@>
        | And(a1, And(a2, a3)) -> <@@ (%%a1 |&| %%a2) |&| %%a3 @@>
        | Equiv(a1, Equiv(a2, a3)) -> <@@ (%%a1 = %%a2) = %%a3 @@>
        | expr -> traverse expr left_assoc

    let rec commute =
        function
        | Or(a1, a2) -> <@@ (%%a2 ||| %%a1) @@>
        | And(a1, a2) -> <@@ (%%a2 |&| %%a1) @@>
        | Equiv(a1, a2) -> <@@ (%%a2 = %%a1) @@>
        | expr -> traverse expr commute

    let rec distrib =
        function
        | Or(a1, And(a2, a3)) -> <@@ %%a1 |&| %%a2 ||| %%a1 |&| %%a3 @@> 
        | Not(And(a1, a2)) -> <@@ not %%a1 ||| not %%a2 @@> 
        | expr -> traverse expr distrib

    let rec collect =
        function
        | Or(And(a1, a2), And(a3, a4)) when sequal a1 a3 -> <@@ %%a1 |&| (%%a2 ||| %%a4) @@>
        | Or(And(a1, a2), And(a3, a4)) when sequal a2 a4 -> <@@ %%a2 |&| (%%a1 ||| %%a3) @@>    
        | Or(Not(a1), Not(a2)) when sequal a1 a2 -> <@@ not(%%a1 |&| %%a2) @@>
        | expr -> traverse expr collect

    /// Reduce equal constants in expression. 
    let Reduce = Rule("Reduce constants in (expression)", reduce_constants)

    /// Expression is left associative.
    let LeftAssoc = Rule("(expression) is left-associative", left_assoc)
    
    /// Expression is right associative.
    let RightAssoc = Rule("(expression) is right-associative", right_assoc)
      
    /// Expression is commutative.
    let Commute = Rule("(expression) is commutative", commute)

    /// Multiplication distributes over addition in expression.
    let Distrib = Rule("Multiplication distributes over addition in (expression)", distrib)
    
    /// Collect distributed terms in expression.
    let Collect = Rule("Collect multiplication terms distributed over addition in (expression)", collect)

    /// Axioms and rules for Boolean algebra.
    let boolean_algebra = ProofSystem(boolean_axioms, [
        Reduce
        LeftAssoc
        RightAssoc
        Commute
        Distrib
        Collect
    ])