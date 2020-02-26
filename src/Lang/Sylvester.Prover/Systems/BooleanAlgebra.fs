namespace Sylph

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.ExprShape

open Sylvester
open FormulaPatterns

module Operators =
    /// Implication operator
    let (==>) (l:bool) (r:bool) = (not l) || r

    /// Equivalence operator. The (==) operator is reserved for conjunctional equivalence
    let (<=>) (l:bool) (r:bool) = l ==> r && r ==> l

module BooleanAlgebra =
    open Operators

    let Taut = F(fun () -> true)

    let Cont = F(fun () -> true)

    (* Patterns*)
    let (|Implies|_|) =
        function
        | SpecificCall <@@ (==>) @@> (None,_,l::r::[]) -> Some(l, r)
        | _ -> None

    let (|Equiv|_|) =
        function
        | SpecificCall <@@ (<=>) @@> (None,_,l::r::[]) -> Some(l, r)
        | _ -> None

    let (|Not|_|) =
        function
        | SpecificCall <@@ not @@> (None,_,l::[]) -> Some l
        | _ -> None

    (* Axioms *)
    // x || y, y || x
    // x && y, y && x
    // x <=> y, y <=> x
    let (|Commute|_|) =
        function
        | Lambda(v1, OrElse(a1, a2)), Lambda(v2, OrElse(b1, b2)) when vequal v1 v2 && sequal2 a1 a2 b2 b1 -> Some true
        | Lambda(v1, AndAlso(a1, a2)), Lambda(v2, AndAlso(b1, b2)) when vequal v1 v2 && sequal2 a1 a2 b2 b1 -> Some true 
        | Lambda(v1, Equiv(a1, a2)), Lambda(v2, Equiv(b1, b2)) when vequal v1 v2 && sequal2 a1 a2 b2 b1 -> Some true 
        | _ -> None

    // x || y || z, x || (y || z)
    // x && y && z, x && (y && z)
    // X <=> y <=> z, x <=> (y <=> z)
    let (|Assoc|_|) =
        function
        | Lambda(v1, OrElse(OrElse(a1, a2), a3)), Lambda(v2, OrElse(b1, OrElse(b2, b3))) when vequal v1 v2 && sequal3 a1 a2 a3 b1 b2 b3 -> Some true        
        | Lambda(v1, OrElse(a1, OrElse(a2, a3))), Lambda(v2, OrElse(OrElse(b1, b2), b3)) when vequal v1 v2 && sequal3 a1 a2 a3 b1 b2 b3 -> Some true
        
        | Lambda(v1, AndAlso(AndAlso(a1, a2), a3)), Lambda(v2, AndAlso(b1, AndAlso(b2, b3))) when vequal v1 v2 && sequal3 a1 a2 a3 b1 b2 b3 -> Some true
        | Lambda(v1, AndAlso(a1, AndAlso(a2, a3))), Lambda(v2, AndAlso(AndAlso(b1, b2), b3)) when vequal v1 v2 && sequal3 a1 a2 a3 b1 b2 b3 -> Some true

        | Lambda(v1, Equiv(Equiv(a1, a2), a3)), Lambda(v2, Equiv(b1, Equiv(b2, b3))) when vequal v1 v2 && sequal3 a1 a2 a3 b1 b2 b3-> Some true         | _ -> None
        | Lambda(v1, Equiv(a1, Equiv(a2, a3))), Lambda(v2, Equiv(Equiv(b1, b2), b3)) when vequal v1 v2 && sequal3 a1 a2 a3 b1 b2 b3-> Some true
        | _ -> None
        
    // x && (y || z), x && y || x && z
    let (|Distrib|_|) =
        function
        | Lambda(v1, OrElse(AndAlso(a1, b1), AndAlso(a2, b2))), Lambda(v2, AndAlso(a3, OrElse(b3, b4))) when vequal v1 v2 && (sequal a1 a2) && (sequal a1 a3) && sequal2 b1 b2 b3 b4 -> Some true
        | Lambda(v1, Not(AndAlso(a1, a2))), Lambda(v2, OrElse(Not(b1), Not(b2))) when vequal v1 v2 && sequal2 a1 a2 b1 b2 -> Some true
        | _ -> None

    let (|Identity|_|) =
        function
        | Lambda(v1, a1), Lambda(v2, a2) when vequal v1 v2 && sequal a1 a2 -> Some true
        | _ -> None

    let (|False|_|) =
        function
        | Lambda(v1, Bool false), Lambda(v2, Not(Bool true)) when vequal v1 v2 -> Some true
        | _ -> None
    
    let (|OrIdentity|_|) = 
        function
        | Lambda(v1, a1), Lambda(v2, OrElse(a2, Bool false)) when vequal v1 v2 && sequal a1 a2 -> Some true
        | Lambda(v1, OrElse(a1, Bool false)), Lambda(v2, a2) when vequal v1 v2 && sequal a1 a2 -> Some true
        | _ -> None

    let (|AndIdentity|_|) = 
        function
        | Lambda(v1, a1), Lambda(v2, AndAlso(a2, Bool true)) when vequal v1 v2 && sequal a1 a2 -> Some true        
        | Lambda(v1, AndAlso(a1, Bool true)), Lambda(v2, a2) when vequal v1 v2 && sequal a1 a2 -> Some true
        | _ -> None

    let boolean_axioms =
        function
        | Commute x
        | Assoc x
        | Distrib x
        | Identity x
        | False x
        | OrIdentity x
        | AndIdentity x -> true
        | _ -> false

    let rec reduce_constants  =
        function
        | OrElse(Bool l, Bool r) -> Expr.Value(l || r)        
        | Not(Bool l) -> Expr.Value(not l)        
        | AndAlso(Bool l, Bool r) -> Expr.Value(l && r)
        | Implies(Bool l, Bool r) -> Expr.Value(l ==> r)
        | Equiv(Bool l, Bool r) -> Expr.Value(l <=> r)
        | expr -> traverse expr reduce_constants

    let rec right_assoc =
        function
        | OrElse(OrElse(a1, a2), a3) -> <@@ %%a1 || (%%a2 || %%a3) @@>
        | AndAlso(AndAlso(a1, a2), a3) -> <@@ %%a1 && (%%a2 && %%a3) @@>
        | Equiv(Equiv(a1, a2), a3) -> <@@ %%a1 <=> (%%a2 <=> %%a3) @@> 
        | expr -> traverse expr right_assoc

    let rec left_assoc =
        function
        | OrElse(a1, OrElse(a2, a3)) -> <@@ (%%a1 || %%a2) || %%a3 @@>
        | AndAlso(a1, AndAlso(a2, a3)) -> <@@ (%%a1 && %%a2) && %%a3 @@>
        | Equiv(a1, Equiv(a2, a3)) -> <@@ (%%a1 ==> %%a2) ==> %%a3 @@>
        | expr -> traverse expr left_assoc

    let rec commute =
        function
        | OrElse(a1, a2) -> <@@ (%%a2 || %%a1) @@>
        | AndAlso(a1, a2) -> <@@ (%%a2 && %%a1) @@>
        | Equiv(a1, a2) -> <@@ (%%a2 <=> %%a1) @@>
        | expr -> traverse expr commute

    let rec distrib =
        function
        | OrElse(a1, AndAlso(a2, a3)) -> <@@ %%a1 && %%a2 || %%a1 && %%a3 @@> 
        | Not(AndAlso(a1, a2)) -> <@@ not %%a1 || not %%a2 @@> 
        | expr -> traverse expr distrib

    let rec collect =
        function
        | OrElse(AndAlso(a1, a2), AndAlso(a3, a4)) when sequal a1 a3 -> <@@ %%a1 && (%%a2 || %%a4) @@>
        | OrElse(AndAlso(a1, a2), AndAlso(a3, a4)) when sequal a2 a4 -> <@@ %%a2 && (%%a1 || %%a3) @@>    
        | OrElse(Not(a1), Not(a2)) when sequal a1 a2 -> <@@ not(%%a1 && %%a2) @@>
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