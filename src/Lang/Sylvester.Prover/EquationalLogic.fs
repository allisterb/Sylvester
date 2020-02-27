namespace Sylph

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.ExprShape

open Sylvester
  
module Operators =
    let (|&|) (l:bool) (r:bool) = l && r
    let (|||) (l:bool) (r:bool) = l || r

/// Formalizes the equational propsitional logic used by Sylph.
/// Based on E: http://www.cs.cornell.edu/home/gries/Logic/Equational.html
/// The main difference is that since we only have to deal with symbolic equality (not mathematical equality)
/// we can drop the restriction that a substitution must replace only variables in an expression 
/// and consider general symbolic substitution.
[<AutoOpen>]
module EquationalLogic =
    open Operators
    (* Patterns *)
    
    // The (=) operator is logical equivalence which is associative i.e we can say a = b = c.
    // The == operator is conjunctional equivalence: A == B == C means A == B and A == C.
    // This is the opposite convention to what Gries et.al adopts for E but we must
    // do it this way because of limitations on how we can use the F# (=) operator. 
    // For Boolean expressions we need to define axioms for both the conjunctive and associative sense.
    let (|Equiv|_|) =
         function
         | SpecificCall <@@ (=) @@> (None,_,l::r::[]) -> Some(l, r)
         | _ -> None

    let (|And|_|)  =
        function
        | SpecificCall <@@ (|&|) @@> (None,_,l::r::[]) -> Some(l,r)
        | _ -> None

    let (|Or|_|) =
        function
        | SpecificCall <@@ (|||) @@> (None,_,l::r::[]) -> Some(l,r)
        | _ -> None


    let (|Not|_|) =
        function
        | SpecificCall <@@ not @@> (None,_,l::[]) -> Some l
        | _ -> None

    (* Axioms *)

    /// Main axiom of Sylph's symbolic equality. A and B are equal if they are symbolically equal.
    /// Since we are only doing string comparisons this law encompasses all 4 of the equational logic laws of equality:
    /// Symmetry, reflexivity, transitivity, and Leibniz's rule: A = B => S(A) = S(B)
    let (|Equal|_|) =
        function
        | (A, B) when sequal A B -> Some true
        | Equiv(A, B), Bool true when sequal A B -> Some true
        | _ -> None

    /// Associativity axiom
    let (|Assoc|_|) =
        function
        // x || y || z == x || (y || z)
        | Or(Or(a1, a2), a3), Or(b1, Or(b2, b3)) when sequal3 a1 a2 a3 b1 b2 b3 -> Some true        
        | Or(a1, Or(a2, a3)), Or(Or(b1, b2), b3) when sequal3 a1 a2 a3 b1 b2 b3 -> Some true
        | Equiv(Or(Or(a1, a2), a3), Or(b1, Or(b2, b3))), Bool true when sequal3 a1 a2 a3 b1 b2 b3 -> Some true        
        | Equiv(Or(a1, Or(a2, a3)), Or(Or(b1, b2), b3)), Bool true when sequal3 a1 a2 a3 b1 b2 b3 -> Some true

        // x && y && z == x && (y && z)    
        | And(And(a1, a2), a3), And(b1, And(b2, b3)) when sequal3 a1 a2 a3 b1 b2 b3 -> Some true
        | And(a1, And(a2, a3)), And(And(b1, b2), b3) when sequal3 a1 a2 a3 b1 b2 b3 -> Some true
        | Equiv(And(And(a1, a2), a3), And(b1, And(b2, b3))), Bool true when sequal3 a1 a2 a3 b1 b2 b3 -> Some true
        | Equiv(And(a1, And(a2, a3)), And(And(b1, b2), b3)), Bool true when sequal3 a1 a2 a3 b1 b2 b3 -> Some true

        // (X = y) = z == x = (y = z)
        | Equiv(Equiv(a1, a2), a3), Equiv(b1, Equiv(b2, b3)) when sequal3 a1 a2 a3 b1 b2 b3-> Some true 
        | Equiv(a1, Equiv(a2, a3)), Equiv(Equiv(b1, b2), b3) when sequal3 a1 a2 a3 b1 b2 b3-> Some true
        | Equiv(Equiv(Equiv(a1, a2), a3), Equiv(b1, Equiv(b2, b3))), Bool true when sequal3 a1 a2 a3 b1 b2 b3-> Some true 
        | Equiv(a1, Equiv(a2, a3)), Equiv(Equiv(b1, b2), b3) when sequal3 a1 a2 a3 b1 b2 b3-> Some true

        | _ -> None

    /// Symmetry axiom
    let (|Symmetry|_|) =
        function
        // x || y == y || x
        | Or(a1, a2), Or(b1, b2) when sequal2 a1 a2 b2 b1 -> Some true
        | Equiv(Or(a1, a2), Or(b1, b2)), Bool true when sequal2 a1 a2 b2 b1 -> Some true

        // x && y == y && x
        | And(a1, a2), And(b1, b2) when sequal2 a1 a2 b2 b1 -> Some true
        | Equiv(And(a1, a2), And(b1, b2)), Bool true when sequal2 a1 a2 b2 b1 -> Some true
        
        // x = y == y = x
        | Equiv(a1, a2), Equiv(b1, b2) when sequal2 a1 a2 b2 b1 -> Some true
        | Equiv(Equiv(a1, a2), Equiv(b1, b2)), Bool true when sequal2 a1 a2 b2 b1 -> Some true
        | _ -> None

    /// Distributive axiom
    let (|Distrib|_|) =
        function
        // x && (y || z) == x && y || x && z
        | And(a3, Or(b3, b4)), Or(And(a1, b1), And(a2, b2)) when sequal a1 a2 && sequal a1 a3 && sequal2 b1 b2 b3 b4 -> Some true
        | Equiv(And(a3, Or(b3, b4)), Or(And(a1, b1), And(a2, b2))), Bool true when sequal a1 a2 && sequal a1 a3 && sequal2 b1 b2 b3 b4 -> Some true
        
        // x && y || x && z == x && (y || z) 
        | Or(And(a1, b1), And(a2, b2)), And(a3, Or(b3, b4)) when  sequal a1 a2 && sequal a1 a3 && sequal2 b1 b2 b3 b4 -> Some true
        | Equiv(Or(And(a1, b1), And(a2, b2)), And(a3, Or(b3, b4))), Bool true when  sequal a1 a2 && sequal a1 a3 && sequal2 b1 b2 b3 b4 -> Some true
        
        // not (x && y) == not x || not y
        | Not(And(a1, a2)), Or(Not(b1), Not(b2)) when sequal2 a1 a2 b1 b2 -> Some true
        | Equiv(Not(And(a1, a2)), Or(Not(b1), Not(b2))), Bool true when sequal2 a1 a2 b1 b2 -> Some true
        | _ -> None
    
    /// Identity axiom
    let (|Identity|_|) = 
        function
        // false = not true
        | Bool false, Not(Bool true) -> Some true
        | Equiv(Bool false, Not(Bool true)), Bool true -> Some true
        
        // x = x || false
        | a1, Or(a2, Bool false) when sequal a1 a2 -> Some true
        | Or(a1, Bool false), a2 when sequal a1 a2 -> Some true
        | Equiv(a1, Or(a2, Bool false)), Bool true when sequal a1 a2 -> Some true
        | Equiv(Or(a1, Bool false), a2), Bool true when sequal a1 a2 -> Some true

        // x = x and true
        | a1, And(a2, Bool true) when sequal a1 a2 -> Some true        
        | And(a1, Bool true), a2 when sequal a1 a2 -> Some true
        | Equiv(a1, And(a2, Bool true)), Bool true when sequal a1 a2 -> Some true        
        | Equiv(And(a1, Bool true), a2), Bool true when sequal a1 a2 -> Some true
        | _ -> None

    let boolean_axioms =
        function
        | Equal x
        | Symmetry x
        | Assoc x
        | Distrib x
        | Identity x -> true
        | _ -> false
