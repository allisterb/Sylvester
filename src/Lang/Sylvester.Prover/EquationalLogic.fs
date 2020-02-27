namespace Sylph

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.ExprShape

open Sylvester

module Operators = 
    /// Implication operator
    let (==>) (l:bool) (r:bool) = (not l) || r  
  
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
    let (|Equiv|_|) =
         function
         | SpecificCall <@@ (=) @@> (None,_,l::r::[]) -> Some(l, r)
         | _ -> None

    let (|Implies|_|) =
        function
        | SpecificCall <@@ (==>) @@> (None,_,l::r::[]) -> Some(l, r)
        | _ -> None


    let (|Not|_|) =
        function
        | SpecificCall <@@ not @@> (None,_,l::[]) -> Some l
        | _ -> None

    (* Axioms *)

    /// Main axiom of Sylph's symbolic equivalence. A and B are equivalent if they are symbolically equivalent.
    /// Since we are only doing string comparisons this law encompasses all 4 of the equational logic laws of equivalence:
    /// Symmetry, reflexivity, transitivity, and Leibniz's rule: A = B => S(A) = S(B)
    let (|Equal|_|) =
        function
        | (A, B) when sequal A B -> Some true
        | _ -> None

    // x or y or z, x or (y or z)
    // x && y && z, x && (y && z)
    // X <=> y <=> z, x <=> (y <=> z)
    let (|Assoc|_|) =
        function
        | OrElse(OrElse(a1, a2), a3), OrElse(b1, OrElse(b2, b3)) when sequal3 a1 a2 a3 b1 b2 b3 -> Some true        
        | OrElse(a1, OrElse(a2, a3)), OrElse(OrElse(b1, b2), b3) when sequal3 a1 a2 a3 b1 b2 b3 -> Some true
        
        | AndAlso(AndAlso(a1, a2), a3), AndAlso(b1, AndAlso(b2, b3)) when sequal3 a1 a2 a3 b1 b2 b3 -> Some true
        | AndAlso(a1, AndAlso(a2, a3)), AndAlso(AndAlso(b1, b2), b3) when sequal3 a1 a2 a3 b1 b2 b3 -> Some true

        | Equiv(Equiv(a1, a2), a3), Equiv(b1, Equiv(b2, b3)) when sequal3 a1 a2 a3 b1 b2 b3-> Some true 
        | Equiv(a1, Equiv(a2, a3)), Equiv(Equiv(b1, b2), b3) when sequal3 a1 a2 a3 b1 b2 b3-> Some true
        | _ -> None

    // x || y, y || x
    // x && y, y && x
    // x <=> y, y <=> x
    let (|Symmetry|_|) =
        function
        | OrElse(a1, a2), OrElse(b1, b2) when sequal2 a1 a2 b2 b1 -> Some true
        | AndAlso(a1, a2), AndAlso(b1, b2) when sequal2 a1 a2 b2 b1 -> Some true 
        | Equiv(a1, a2), Equiv(b1, b2) when sequal2 a1 a2 b2 b1 -> Some true 
        | _ -> None

    // x && (y || z), x && y || x && z
    let (|Distrib|_|) =
        function
        | OrElse(AndAlso(a1, b1), AndAlso(a2, b2)), AndAlso(a3, OrElse(b3, b4)) when  sequal a1 a2 && sequal a1 a3 && sequal2 b1 b2 b3 b4 -> Some true
        | Not(AndAlso(a1, a2)), OrElse(Not(b1), Not(b2)) when sequal2 a1 a2 b1 b2 -> Some true
        | _ -> None
    
    let (|Identity|_|) = 
        function
        | Bool false, Not(Bool true) -> Some true
        | a1, OrElse(a2, Bool false) when sequal a1 a2 -> Some true
        | OrElse(a1, Bool false), Lambda(v2, a2) when sequal a1 a2 -> Some true
        | a1, AndAlso(a2, Bool true) when sequal a1 a2 -> Some true        
        | AndAlso(a1, Bool true), a2 when sequal a1 a2 -> Some true
        | _ -> None

    let boolean_axioms =
        function
        | Equal x
        | Symmetry x
        | Assoc x
        | Distrib x
        | Identity x -> true
        | _ -> false
