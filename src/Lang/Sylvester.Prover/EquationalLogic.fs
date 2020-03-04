namespace Sylph

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.ExprShape

open Sylvester
  
/// Logical operators for formulas.
module Operators =
    let (!!) (l:bool)  = not l
    let (|&|) (l:bool) (r:bool) = l && r
    let (|||) (l:bool) (r:bool) = l || r
    let (!=) (l:bool) (r:bool) = not(l = r)
    let (==>) (l:bool) (r:bool) = (not l) || r

/// Formalizes the equational logic used by Sylph called S.
/// Based on E: http://www.cs.cornell.edu/home/gries/Logic/Equational.html
/// The main difference is that since we only have to deal with symbolic equality (not mathematical equality)
/// we can drop the restriction that a substitution must replace only variables in an expression 
/// and consider general textual substitution with syntactically valid expressions.
module EquationalLogic =
    open Operators

    (* Patterns *)
    
    // The (=) operator is logical equivalence which is associative i.e we can say a = b = c.
    // The == operator is conjunctional equality: A == B == C means A == B and A == C.
    // This is the opposite convention to what Gries et.al adopts for E but we must
    // do it this way because of limitations on how we can use the F# (=) operator. 
    let (|Equiv|_|) =
         function
         | SpecificCall <@@ (=) @@> (None,_,l::r::[]) -> Some(l, r)
         | _ -> None
        
    // We need to define axioms for both the conjunctive and associative sense of =.
    let (|Conj|_|) =
        function
        | Equiv(expr2), Bool true -> Some expr2
        | _ -> None

    // We need to define axioms symmetrically because of the symmetric properties of ==.
    let (|Symm|):(Expr * Expr)->(Expr * Expr) =
        function
        | (A, B) -> (B, A)
       
    let (|NotEquiv|_|) =
         function
         | SpecificCall <@@ (!=) @@> (None,_,l::r::[]) -> Some(l, r)
         | _ -> None

    let (|And|_|)  =
        function
        | SpecificCall <@@ (|&|) @@> (None,_,l::r::[]) -> Some(l,r)
        | _ -> None

    let (|Or|_|) =
        function
        | SpecificCall <@@ (|||) @@> (None,_,l::r::[]) -> Some(l,r)
        | _ -> None

    let (|Implies|_|) =
        function
        | SpecificCall <@@ (==>) @@> (None,_,l::r::[]) -> Some(l,r)
        | _ -> None

    let (|Not|_|) =
        function
        | SpecificCall <@@ (!!) @@> (None,_,l::[]) -> Some l
        | SpecificCall <@@ not @@> (None,_,l::[]) -> Some l
        | _ -> None

    (* Axioms *)

    /// Main axiom of Sylph's symbolic equality. A and B are equal if they are: 
    /// * Syntactically valid F# expressions
    /// * Decomposed to the same sequence of symbols i.e. strings.
    /// Since we are only concerned with string equality this law encompasses all 4 of the equational logic laws of equality:
    /// Symmetry, reflexivity, transitivity, and Leibniz's rule: A = B => S(A) = S(B)
    let (|Equal|_|) =
        function
        | (A, B) when sequal A B -> Some B
        | _ -> None

    /// Associativity axioms
    let (|Assoc|_|) =
        function
        // x ||| y ||| z == x ||| (y ||| z)
        | Or(Or(a1, a2), a3), Or(b1, Or(b2, b3)) when sequal3 a1 a2 a3 b1 b2 b3 -> Some <@@ %%b1 ||| (%%b2 ||| %%b3) @@>        
        
        // x && y && z == x && (y && z)    
        | And(And(a1, a2), a3), And(b1, And(b2, b3)) when sequal3 a1 a2 a3 b1 b2 b3 -> Some <@@ %%b1 |&| (%%b2 |&| %%b3) @@>

        // (X = y) = z == x = (y = z)
        | Equiv(Equiv(a1, a2), a3), Equiv(b1, Equiv(b2, b3)) when sequal3 a1 a2 a3 b1 b2 b3-> Some <@@ (%%b1: bool) = ((%%b2:bool) = (%%b3:bool)) @@> 
        
        | _ -> None

    /// Symmetry axioms
    let (|Symmetry|_|) =
        function
        // x ||| y == y ||| x
        | Or(a1, a2), Or(b1, b2) when sequal2 a1 a2 b2 b1 -> Some <@@ %%b1 ||| %%b2 @@>

        // x && y == y && x
        | And(a1, a2), And(b1, b2) when sequal2 a1 a2 b2 b1 -> Some <@@ %%b1 |&| %%b2 @@>
        
        // x = y == y = x
        | Equiv(a1, a2), Equiv(b1, b2) when sequal2 a1 a2 b2 b1 -> Some <@@ (%%b1: bool) = (%%b2: bool) @@>
        
        // x != y == y != x
        | NotEquiv(a1, a2), NotEquiv(b1, b2) when sequal2 a1 a2 b2 b1 -> Some <@@ (%%b1: bool) != (%%b2: bool) @@>
        
        | _ -> None

    /// Distributivity axioms
    let (|Distrib|_|) =
        function
        // x && (y || z) == x && y || x && z
        | And(a3, Or(b3, b4)), Or(And(a1, b1), And(a2, b2)) when sequal a1 a2 && sequal a1 a3 && sequal2 b1 b2 b3 b4 -> Some <@@ (%%a1 && %%b1) ||| (%%a2 && %%b2) @@>
                
        // p ||| (q = r) == p = q ||| p = r
        | Or(a1, Equiv(a2, a3)), Or(Equiv(b1, b2), Equiv(b3, b4)) when sequal a1 b1 && sequal a1 b3 && sequal a2 b2 && sequal a3 b4 -> Some <@@ (%%b1 = %%b2) ||| (%%b3 = %%b4) @@>

        // not (x |&| y) == not x ||| not y
        | Not(And(a1, a2)), Or(Not(b1), Not(b2)) when sequal2 a1 a2 b1 b2 -> Some <@@ not %%b1 ||| not %%b2 @@>
        
        // not (p = q) == not p = q
        | Not(Equiv(a1, a2)), Equiv(Not(a3), a4) when sequal2 a1 a2 a3 a4 -> Some <@@ not (%%a3: bool) = (%%a4: bool) @@>
        
        // p != q == not p = q
        | NotEquiv(a1, a2), Equiv(Not(a3), a4) when sequal2 a1 a2 a3 a4 -> Some <@@ not (%%a3: bool) = (%%a4: bool) @@>
        
        | _ -> None

    /// Identity axioms
    let (|Identity|_|) = 
        function
        // x = x == true
        | Equiv(a1, a2), Bool true when sequal a1 a2 -> Some <@@ true @@>
                
        // false = not true
        | Bool false, Not(Bool true) -> Some <@@ not true @@>
        
        // x = x || false
        | a1, Or(a2, Bool false) when sequal a1 a2 -> Some <@@ %%a2 ||| false @@>

        // x = x and true
        | a1, And(a2, Bool true) when sequal a1 a2 -> Some <@@ %%a2 |&| true @@>        
        
        // x != y == !(x = y)
        | NotEquiv(a1, a2), Not(Equiv(a3, a4)) when sequal2 a1 a2 a3 a4 -> Some <@@ true @@>
        
        | _ -> None

    /// Idempotent axioms
    let (|Idempotent|_|) = 
        function
        | Or(a1, a2), a3 when sequal a1 a2 && sequal a1 a3 -> Some <@@ (%%a3:bool) @@>
        | _ -> None

    /// Axiom of the excluded middle
    let (|ExcludedMiddle|_|) =
        function
        | Or(a1, Not(a2)), Bool true when sequal a1 a2 -> Some <@@ (%%a2: bool) @@>
        | _ -> None

    /// Golden rule axiom
    let (|GoldenRule|_|) =
        function
        | Equiv(Equiv(Equiv(And(p1, q1), p2), q2), Or(p3, q3)), Bool true when sequal p1 p2 && sequal p2 p3 && sequal q1 q2 && sequal q2 q3 -> Some <@@ true @@>
        | _ -> None

    let (|ConjLogicalAxioms|_|) =
        function
        | Conj(Equal x) 
        | Conj(Symmetry x) 
        | Conj(Assoc x) 
        | Conj(Distrib x) 
        | Conj(Identity x) 
        | Conj(Idempotent x) 
        | Conj(ExcludedMiddle x) 
        | Conj(GoldenRule x) -> Some x
        | _ -> None

    let (|SymmLogicalAxioms|_|) =
        function
        | Symm(A, B) -> match (A, B) with |Equal x|Symmetry x|Assoc x|Distrib x|Identity x|Idempotent x|ExcludedMiddle x|GoldenRule x -> Some x | _ -> None

    let logical_axioms =
        function
        | Equal x
        | Symmetry x
        | Assoc x
        | Distrib x 
        | Identity x 
        | Idempotent x
        | ExcludedMiddle x
        | GoldenRule x
        | ConjLogicalAxioms x 
        | SymmLogicalAxioms x -> true
        | _ -> false

    (* Inference rules *) 
    
    /// Reduce logical constants.
    let rec reduce_constants  =
        function
        | Or(Bool l, Bool r) -> Expr.Value(l ||| r)        
        | Not(Bool l) -> Expr.Value(not l)        
        | And(Bool l, Bool r) -> Expr.Value(l |&| r)
        | Implies(Bool l, Bool r) -> Expr.Value(l ==> r)
        | Equiv(Bool l, Bool r) -> Expr.Value((l = r))
        | expr -> traverse expr reduce_constants
    
    /// Logical operators are right associative.
    let rec right_assoc =
        function
        | Or(Or(a1, a2), a3) -> <@@ %%a1 ||| (%%a2 ||| %%a3) @@>
        | And(And(a1, a2), a3) -> <@@ %%a1 |&| (%%a2 |&| %%a3) @@>
        | Equiv(Equiv(a1, a2), a3) -> <@@ (%%a1:bool) = ((%%a2:bool) = (%%a3:bool)) @@> 
        | expr -> traverse expr right_assoc
    
    /// Logical operators are left associative.
    let rec left_assoc =
        function
        | Or(a1, Or(a2, a3)) -> <@@ (%%a1 ||| %%a2) ||| %%a3 @@>
        | And(a1, And(a2, a3)) -> <@@ (%%a1 |&| %%a2) |&| %%a3 @@>
        | Equiv(a1, Equiv(a2, a3)) -> <@@ ((%%a1:bool) = (%%a2:bool)) = (%%a3:bool) @@>
        | expr -> traverse expr left_assoc
    
    /// Logical operators commute.
    let rec commute =
        function
        | Or(a1, a2) -> <@@ %%a2 ||| %%a1 @@>
        | And(a1, a2) -> <@@ %%a2 |&| %%a1 @@>
        | Equiv(a1, a2) -> <@@ (%%a2:bool) = (%%a1:bool) @@>
        | Not(Or(a1, a2)) -> <@@ not (%%a2 ||| %%a1) @@>
        | Not(And(a1, a2)) -> <@@ not (%%a2 |&| %%a1) @@>
        | Not(Equiv(a1, a2)) -> <@@ not ((%%a2:bool) = (%%a1:bool)) @@>
        | expr -> traverse expr commute
    
    /// Distribute logical terms.
    let rec distrib =
        function
        | Or(a1, And(a2, a3)) -> <@@ %%a1 |&| %%a2 ||| %%a1 |&| %%a3 @@> 
        | Or(a1, Equiv(a2, a3)) -> <@@ ((%%a1)  ||| (%%a2)) = ((%%a1) ||| (%%a3)) @@> 
        | Not(And(a1, a2)) -> <@@ not %%a1 ||| not %%a2 @@>
        | expr -> traverse expr distrib
    
    /// Collect distributed logical terms.
    let rec collect =
        function
        | Or(And(a1, a2), And(a3, a4)) when sequal a1 a3 -> <@@ %%a1 |&| (%%a2 ||| %%a4) @@>
        | Or(And(a1, a2),  And(a3, a4)) when sequal a2 a4 -> <@@ %%a2 |&| (%%a1 ||| %%a3) @@>    
        | Or(Not(a1), Not(a2)) when sequal a1 a2 -> <@@ not(%%a1 |&| %%a2) @@>
        | Equiv(Not a1, a2)  -> <@@ not((%%a1:bool) = (%%a2:bool)) @@>
        | expr -> traverse expr collect
    
    /// ||| operator is idempotent    
    let rec idemp =
        function
        | Or(a1, a2) when sequal a1 a2 -> <@@ (%%a2:bool) @@>
        | expr -> traverse expr idemp

    let rec excluded_middle =
        function
        | Or(a1, Not(a2)) when sequal a1 a2 -> <@@ true @@>
        | expr -> traverse expr excluded_middle

    let rec golden_rule =
        function
        | Equiv(Equiv(Equiv(And(p1, q1), p2), q2), Or(p3, q3))  -> <@@ true @@>
        | expr -> traverse expr golden_rule