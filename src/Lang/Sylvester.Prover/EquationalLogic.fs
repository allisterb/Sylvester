namespace Sylvester

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Patterns
open Descriptions

/// Formalizes the default equational propsitional logic used by Sylph called S.
/// Based on E: https://www.cs.cornell.edu/fbs/publications/94-1455.pdf
///             http://www.cs.cornell.edu/home/gries/Logic/Equational.html
/// The main difference is that since we only have to deal with symbolic equality (not mathematical equality)
/// we can drop the restriction that a substitution must replace only variables in an expression 
/// and consider general textual substitution with syntactically valid expressions.0
module EquationalLogic =
    /// Print Unicode logical operator symbols
    let print_S_Operators (s:string) = 
        s.Replace("|||", "\u2228")
         .Replace("|&|", "\u2227")
         .Replace("|-", "\u22A2")
         .Replace(" not ", " \u00AC ")
         .Replace("not ", "\u00AC ")

    let desc = axiom_desc "Equational Logic" print_S_Operators
    
    (* Patterns *)

    /// true = p = p
    let (|TruthDefn|_|) =
        function
        | Equals(Bool true, Equals(a1, a2)) when sequal a1 a2 -> Some (pattern_desc "Definition of true" <@fun x -> x = x = true @>)
        | _ -> None

    /// false = not ture
    let (|FalseDefn|_|) =
        function
        | Equals(Bool false, Not(Bool true)) -> Some (pattern_desc "Definition of false" <@fun x -> x = x = true @>)
        | _ -> None

    let (|NotDistrib|_|) =
        function
        | Equals(NotEquals(a1, a2), Equals(Not(b1), b2)) when sequal2 a1 a2 b1 b2 -> Some (pattern_desc "Distributivity" <@fun x y -> not(x = y) = (not x = y) @>)
        | _ -> None

    /// p ||| not p
    let (|ExcludedMiddle|_|) =
        function
        | Or(a1, Not(a2)) when sequal a1 a2 -> Some (pattern_desc "the Excluded Middle" <@fun x -> x ||| not x @>)
        | _ -> None

    /// p |&| q = p = q ||| p = q 
    let (|GoldenRule|_|) =
        function
        | Equals(And(p1, q1), Equals(Equals(p2, q2), Or(p3, q3))) when sequal p1 p2 && sequal p1 p3 && sequal q1 q2 && sequal q2 q3 -> 
                                                                Some (pattern_desc "the Golden Rule" <@fun x y -> x |&| y = (x = y) = (x ||| y) @>)
        | _ -> None

    /// p => q = p ||| q ||| q
    let (|Implication|_|) =
        function
        | Equals(Implies(a1, a2), Equals(Or(a3, a4), a5)) when sequal a1 a3 && sequal a2 a4 && sequal a4 a5 -> 
                                                                Some (pattern_desc "Implication" <@fun x y-> (x ==> y) = ((x ||| y) = y)@>)
        | Equals(Conseq(a1, a2), Implies(a3, a4)) when sequal2 a1 a2 a4 a3 -> 
                                                                Some (pattern_desc "Consequence" <@fun x y -> (x <== y) = (y ==> x) @>)
        | _ -> None

    let equational_logic_axioms = 
        function
        | SEqual x

        | TruthDefn x
        | FalseDefn x
        | Reflex <@ (=) @> x

        | Assoc <@(=)@> <@ (=) @> x
        | Assoc <@(=)@> <@ (|&|) @> x
        | Assoc <@(=)@> <@ (|||) @> x 
        
        | Commute <@(=)@> <@ (=) @> x
        | Commute <@(=)@> <@ (|&|) @> x
        | Commute <@(=)@> <@ (|||) @> x 
       
        | NotDistrib x
        | Distrib <@(=)@> <@ (|&|) @> <@ (|||) @> x  // x && (y || z) = x && y || x && z 
        | Distrib <@(=)@> <@ (|||) @> <@ (=) @> x  // x ||| (y = z) = x ||| y = x ||| z
        
        | UnaryDistrib <@(=)@> <@ not @> <@ (|&|) @> x  // not (x |&| y) = not x ||| not y
        | UnaryDistrib <@(=)@> <@ not @> <@ (=) @> x 
       
        | Identity <@(=)@> <@ (=) @> <@ true @> x
        | Identity <@(=)@> <@ (|&|) @> <@ true @> x
        | Identity <@(=)@> <@ (|||) @> <@ false @> x
                
        | Duality <@(=)@> <@ (=) @> <@ (<>) @> <@ not @> x
        | Duality <@(=)@> <@ (<>) @> <@ (=) @> <@ not @> x

        | Idempotency <@(=)@> <@ (|&|) @> x
        | Idempotency <@(=)@> <@ (|||) @> x 
        
        | ExcludedMiddle x
        | GoldenRule x
        | Implication x -> Some (desc x)
        | _ -> None

    
    (* Inference rules *) 
    
    /// Reduce logical constants.
    let rec reduce_constants  =
        function
        | Equals(Bool l, Bool r) -> Expr.Value((l = r))
        | Not(Bool l) -> Expr.Value(not l)
        | Or(Bool l, Bool r) -> Expr.Value(l ||| r)                
        | And(Bool l, Bool r) -> Expr.Value(l |&| r)
        | Implies(Bool l, Bool r) -> Expr.Value(l ==> r)
        | expr -> traverse expr reduce_constants
    
    /// Logical operators are right associative.
    let rec right_assoc =
        function
        | Or(Or(a1, a2), a3) -> <@@ %%a1 ||| (%%a2 ||| %%a3) @@>
        | And(And(a1, a2), a3) -> <@@ %%a1 |&| (%%a2 |&| %%a3) @@>
        | Equals(Equals(a1, a2), a3) -> <@@ (%%a1:bool) = ((%%a2:bool) = (%%a3:bool)) @@> 
        | expr -> traverse expr right_assoc
    
    /// Logical operators are left associative.
    let rec left_assoc =
        function
        | Or(a1, Or(a2, a3)) -> <@@ (%%a1 ||| %%a2) ||| %%a3 @@>
        | And(a1, And(a2, a3)) -> <@@ (%%a1 |&| %%a2) |&| %%a3 @@>
        | Equals(a1, Equals(a2, a3)) -> <@@ ((%%a1:bool) = (%%a2:bool)) = (%%a3:bool) @@>
        | expr -> traverse expr left_assoc
    
    /// Logical operators commute.
    let rec commute =
        function
        | Or(a1, a2) -> <@@ %%a2 ||| %%a1 @@>
        | And(a1, a2) -> <@@ %%a2 |&| %%a1 @@>
        | Equals(a1, a2) -> <@@ (%%a2:bool) = (%%a1:bool) @@>
        | Not(Or(a1, a2)) -> <@@ not (%%a2 ||| %%a1) @@>
        | Not(And(a1, a2)) -> <@@ not (%%a2 |&| %%a1) @@>
        | Not(Equals(a1, a2)) -> <@@ not ((%%a2:bool) = (%%a1:bool)) @@>
        | expr -> traverse expr commute
    
    /// Distribute logical terms.
    let rec distrib =
        function
        | Or(a1, And(a2, a3)) -> <@@ %%a1 |&| %%a2 ||| %%a1 |&| %%a3 @@> 
        | Or(a1, Equals(a2, a3)) -> <@@ ((%%a1)  ||| (%%a2)) = ((%%a1) ||| (%%a3)) @@> 
        | Not(And(a1, a2)) -> <@@ not %%a1 ||| not %%a2 @@>
        | expr -> traverse expr distrib
    
    /// Collect distributed logical terms.
    let rec collect =
        function
        | Or(And(a1, a2), And(a3, a4)) when sequal a1 a3 -> <@@ %%a1 |&| (%%a2 ||| %%a4) @@>
        | Or(And(a1, a2),  And(a3, a4)) when sequal a2 a4 -> <@@ %%a2 |&| (%%a1 ||| %%a3) @@>    
        | Or(Not(a1), Not(a2)) when sequal a1 a2 -> <@@ not(%%a1 |&| %%a2) @@>
        | Equals(Not a1, a2)  -> <@@ not((%%a1:bool) = (%%a2:bool)) @@>
        | expr -> traverse expr collect
    
    /// ||| operator is idempotent.    
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
        | Equals(Equals(Equals(And(p1, q1), p2), q2), Or(p3, q3))  -> <@@ true @@>
        | expr -> traverse expr golden_rule