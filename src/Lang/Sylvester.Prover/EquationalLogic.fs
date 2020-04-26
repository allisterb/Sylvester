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
/// and consider general textual substitution with syntactically valid expressions.
/// The number after each axiom corresponds to the number of the axiom in the textbook A Logical Approach to Discrete Math by Gries et.al.
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
    let (|DefTrue|_|) =
        function
        | Equals(Bool true, Equals(a1, a2)) when sequal a1 a2 -> pattern_desc "Definition of true" <@fun x -> x = x = true @> |> Some
        | Bool true -> pattern_desc "Definition of true" <@ true @> |> Some // This isn't defined as an axiom in E but is included here for convenience.
        | _ -> None

    /// false = not true
    let (|DefFalse|_|) =
        function
        | Equals(Bool false, Not(Bool true)) -> pattern_desc "Definition of false" <@ false = not true @> |> Some
        | Not(Bool false) -> pattern_desc "Definition of false" <@ not true @> |> Some // This isn't defined as an axiom in E but is included here for convenience.
        | _ -> None

    /// not (p = q) = not p = q
    let (|DistribNot|_|) =
        function
        | Equals(Not(Equals(a1, a2)), Equals(Not(b1), b2)) when sequal2 a1 a2 b1 b2 -> pattern_desc "Distributivity" <@fun x y -> not(x = y) = (not x = y) @> |> Some
        | _ -> None

    /// p ||| not p
    let (|ExcludedMiddle|_|) =
        function
        | Or(a1, Not(a2)) when sequal a1 a2 -> pattern_desc "the Excluded Middle" <@fun x -> x ||| not x @> |> Some
        | _ -> None

    /// p |&| q = p = q = p ||| q 
    let (|GoldenRule|_|) =
        function
        | Equals(And(p1, q1), Equals(Equals(p2, q2), Or(p3, q3))) when sequal p1 p2 && sequal p2 p3 && sequal q1 q2 && sequal q2 q3 -> 
                                                                pattern_desc "the Golden Rule" <@fun x y -> x |&| y = (x = y) = (x ||| y) @> |> Some
        | _ -> None

    /// p ==> q = p ||| q ||| q
    let (|Implication|_|) =
        function
        | Equals(Implies(a1, a2), Equals(Or(a3, a4), a5)) when sequal a1 a3 && sequal a2 a4 && sequal a4 a5 -> 
                                                                pattern_desc "Implication" <@fun x y-> (x ==> y) = ((x ||| y) = y)@> |> Some
        | Equals(Conseq(a1, a2), Implies(a3, a4)) when sequal2 a1 a2 a4 a3 -> 
                                                                pattern_desc "Consequence" <@fun x y -> (x <== y) = (y ==> x) @> |> Some
        | _ -> None

    (* Axioms *)

    let equational_logic_axioms = 
        function
        | SEqual x
        | DefTrue x // (3.3)
        | DefFalse x //(3.8)
        | BinaryOpDef <@ (=) @> <@ (<>) @> <@ (=) @> <@ not @> x // (3.10)
        
        | Assoc <@(=)@> <@ (=) @> x  // (3.1)
        | Assoc <@(=)@> <@ (|||) @> x // (3.25)
        
        | Symm <@ (=) @> x // (3.2)
        | Commute <@ (=) @> <@ (|||) @> x // (3.24)

        | DistribNot x // (3.9) 
        | Distrib <@(=)@> <@ (|||) @> <@ (=) @> x  // (3.27)      
       
        | Idempotency <@(=)@> <@ (|||) @> x // (3.26)
        
        | ExcludedMiddle x // (3.28)
        | GoldenRule x // (3.35)
        | Implication x -> Some (desc x)
        | _ -> None

    (* Expression tree functions for rules *) 
    
    /// Reduce logical constants.
    let rec reduce_constants  =
        function
        | Equals(Bool l, Bool r) -> Expr.Value((l = r))
        | NotEquals(Bool l, Bool r) -> Expr.Value(l <> r)
        | Not(Bool l) -> Expr.Value(not l)
        | Or(Bool l, Bool r) -> Expr.Value(l ||| r)                
        | And(Bool l, Bool r) -> Expr.Value(l |&| r)
        | Implies(Bool l, Bool r) -> Expr.Value(l ==> r)
        | expr -> traverse expr reduce_constants
    
    /// Logical operators are right associative.
    let rec right_assoc =
        function
        | Equals(Equals(a1, a2), a3) -> <@@ (%%a1:bool) = ((%%a2:bool) = (%%a3:bool)) @@>
        | Or(Or(a1, a2), a3) -> <@@ %%a1 ||| (%%a2 ||| %%a3) @@>
        | And(And(a1, a2), a3) -> <@@ %%a1 |&| (%%a2 |&| %%a3) @@> // Include associativity of and as an admitted rule since the formal proof is long
        | expr -> traverse expr right_assoc
    
    /// Logical operators are left associative.
    let rec left_assoc =
        function
        | Equals(a1, Equals(a2, a3)) -> <@@ ((%%a1:bool) = (%%a2:bool)) = (%%a3:bool) @@>
        | Or(a1, Or(a2, a3)) -> <@@ (%%a1 ||| %%a2) ||| %%a3 @@>
        | And(a1, And(a2, a3)) -> <@@ (%%a1 |&| %%a2) |&| %%a3 @@> // Include associativity of and as an admitted rule since the formal proof is long
        | expr -> traverse expr left_assoc
    
    /// Logical operators commute.
    let rec commute =
        function
        | Equals(a1, a2) -> <@@ (%%a2:bool) = (%%a1:bool) @@>
        | Or(a1, a2) -> <@@ %%a2 ||| %%a1 @@>
        | expr -> traverse expr commute
    
    /// Distribute logical terms.
    let rec distrib =
        function
        | Not(Equals(a1, a2)) -> <@@ not %%a1 = %%a2 @@>
        | Or(a1, Equals(a2, a3)) -> <@@ ((%%a1)  ||| (%%a2)) = ((%%a1) ||| (%%a3)) @@>
        | Or(a1, Or(a2, a3)) -> <@@ ((%%a1)  ||| (%%a2)) ||| ((%%a1) ||| (%%a3)) @@>
        | expr -> traverse expr distrib
    
    /// Collect distributed logical terms.
    let rec collect =
        function
        | Equals(Not a1, a2)  -> <@@ not((%%a1:bool) = (%%a2:bool)) @@>
        | Equals(Or(a1, a2), Or(a3, a4)) when sequal a1 a3 -> <@@ %%a1 ||| (%%a2 = %%a4) @@>
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
        | And(p, q) -> <@@ (%%p:bool) = (%%q:bool) = ((%%p:bool) ||| (%%q:bool)) @@>
        | expr -> traverse expr golden_rule