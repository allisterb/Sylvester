namespace Sylph

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Sylvester
open FormulaPatterns
    
/// Text description of axioms.
[<AutoOpen>]
module AxiomDescriptions = 
    /// Text description of axioms.
    type AxiomDescription = AxiomDescription of string * string with
        member x.Name = let (AxiomDescription(n, d)) = x in n
        member x.Description = let (AxiomDescription(n, d)) = x in d
    
    /// Print Unicode logical operator symbols
    let print_S_Operators (s:string) = 
        s.Replace("|||", "\u2228")
         .Replace("|&|", "\u2227")
         .Replace("==", "\u2261")
         .Replace("|-", "\u22A2")
         .Replace(" not ", " \u00AC ")
         .Replace("not ", "\u00AC ")
       
    /// Create an axiom description from a name and an example formula.
    let axiom_desc name example  = AxiomDescription(name, example |> body |> src |> print_S_Operators)

/// Formalizes the default equational propsitional logic used by Sylph called S.
/// Based on E: https://www.cs.cornell.edu/fbs/publications/94-1455.pdf
///             http://www.cs.cornell.edu/home/gries/Logic/Equational.html
/// The main difference is that since we only have to deal with symbolic equality (not mathematical equality)
/// we can drop the restriction that a substitution must replace only variables in an expression 
/// and consider general textual substitution with syntactically valid expressions.0
module EquationalLogic =
    (* Axioms *)

    /// Main axiom of Sylph's symbolic equality. A and B are equal if they are: 
    /// * Syntactically valid F# expressions
    /// * Decomposed to the same sequence of symbols i.e. strings.
    /// Since we are only concerned with string equality this law encompasses all 4 of the equational logic laws of equality:
    /// Symmetry, reflexivity, transitivity, and Leibniz's rule: A = B => S(A) = S(B)
    let (|SEqual|_|) =
        function
        | (A, B) when sequal A B -> Some (axiom_desc "Equality" <@ fun x y -> x  = y @>)
        | _ -> None

    /// Associativity axioms
    let (|Assoc|_|) =
        function
        // x ||| y ||| z == x ||| (y ||| z)
        | Or(Or(a1, a2), a3), Or(b1, Or(b2, b3)) when sequal3 a1 a2 a3 b1 b2 b3 -> Some (axiom_desc "Associativity" <@ fun x y z -> x ||| y ||| z = x ||| (y ||| z) @>)        
        
        // x && y && z == x && (y && z)    
        | And(And(a1, a2), a3), And(b1, And(b2, b3)) when sequal3 a1 a2 a3 b1 b2 b3 -> Some (axiom_desc "Associativity" <@ fun x y z -> x |&| y |&| z = x |&| (y |&| z) @>)

        // (X = y) = z == x = (y = z)
        | Equiv(Equiv(a1, a2), a3), Equiv(b1, Equiv(b2, b3)) when sequal3 a1 a2 a3 b1 b2 b3-> Some (axiom_desc "Associativity" <@ fun x y z -> (x = y) = z = x = (y = z) @>)
        
        | _ -> None

    /// Symmetry axioms
    let (|Symmetry|_|) =
        function
        // x ||| y == y ||| x
        | Or(a1, a2), Or(b1, b2) when sequal2 a1 a2 b2 b1 -> Some (axiom_desc "Symmetry" <@ fun x y -> x ||| y = y ||| x @>)

        // x && y == y && x
        | And(a1, a2), And(b1, b2) when sequal2 a1 a2 b2 b1 -> Some (axiom_desc "Symmetry" <@ fun x y -> x |&| y = y |&| x @>)
        
        // x = y == y = x
        | Equiv(a1, a2), Equiv(b1, b2) when sequal2 a1 a2 b2 b1 -> Some (axiom_desc "Symmetry" <@ fun x y -> x = y = y = x @>)
        
        // x <> y == y <> x
        | NotEquiv(a1, a2), NotEquiv(b1, b2) when sequal2 a1 a2 b2 b1 -> Some (axiom_desc "Symmetry" <@ fun x y -> x = y = y = x @>)
        
        | _ -> None

    /// Distributivity axioms
    let (|Distrib|_|) =
        function
        // x && (y || z) == x && y || x && z
        | And(a3, Or(b3, b4)), Or(And(a1, b1), And(a2, b2)) when sequal a1 a2 && sequal a1 a3 && sequal2 b1 b2 b3 b4 -> Some (axiom_desc "Distributive" <@ fun x y z -> x |&| (y ||| z) = x && y ||| x |&| z @>)
                
        // p ||| (q = r) == p ||| q == p ||| r
        | Or(a1, Equiv(a2, a3)), Equiv(Or(b1, b2), Or(b3, b4)) when sequal a1 b1 && sequal a1 b3 && sequal a2 b2 && sequal a3 b4 -> Some (axiom_desc "Distributive" <@ fun x y z -> x ||| (y = z) = x ||| y = x ||| z @>)

        // not (x |&| y) == not x ||| not y
        | Not(And(a1, a2)), Or(Not(b1), Not(b2)) when sequal2 a1 a2 b1 b2 -> Some (axiom_desc "Distributive" <@ fun x y -> not (x |&| y) = not x ||| not y @>)
        
        // not (p = q) == not p = q
        | Not(Equiv(a1, a2)), Equiv(Not(a3), a4) when sequal2 a1 a2 a3 a4 -> Some (axiom_desc "Distributive" <@ fun x y -> not (x = y) = not x = y @>)
                
        | _ -> None

    /// Identity axioms
    let (|Identity|_|) = 
        function
        // x = x == true
        | Equiv(a1, a2), Bool true when sequal a1 a2 -> Some (axiom_desc "Identity" <@fun x -> (x = x) = true @>)
    
        // false == not true
        | Bool false, Not(Bool true) -> Some (axiom_desc "Identity" <@fun () -> false = not true @>)
        
        // x == x || false
        | a1, Or(a2, Bool false) when sequal a1 a2 -> Some (axiom_desc "Identity" <@fun x -> x = (x ||| false) @>)

        // x == x and true
        | a1, And(a2, Bool true) when sequal a1 a2 -> Some (axiom_desc "Identity" <@fun x -> x = (x |&| true) @>)       
                
        // x <> y == not (x = y)
        | NotEquiv(a1, a2), Not(Equiv(a3, a4)) when sequal2 a1 a2 a3 a4 -> Some (axiom_desc "Identity" <@fun x y -> (x <> y) = not(x = y) @>)
        
        | _ -> None
    
    /// Duality axioms
    let (|Duality|_|) =
        function
        | Equiv(a1, a2), Not(NotEquiv(Not(a3), Not(a4))) when sequal2 a1 a2 a3 a4 -> Some (axiom_desc "Duality" <@fun x y -> (x = y) = not(not x = not y) @>)
        | Bool true, Not(Bool false) -> Some (axiom_desc "Duality" <@fun () -> true = not false @>)
        | Or(a1, a2), Not(And(Not(a3), Not(a4))) when sequal2 a1 a2 a3 a4 -> Some (axiom_desc "Duality" <@fun x y -> x ||| y = not (not x |&| not y) @>)
        | And(a1, a2), Not(Or(Not(a3), Not(a4))) when sequal2 a1 a2 a3 a4 -> Some (axiom_desc "Duality" <@fun x y -> x |&| y = not (not x ||| not y) @>)
        | _ -> None

    /// Idempotent axioms.
    let (|Idempotent|_|) = 
        function
        | And(a1, a2), a3 when sequal a1 a2 && sequal a1 a3 -> Some (axiom_desc "Idempotent" <@fun x -> x |&| x = x @>)
        | Or(a1, a2), a3 when sequal a1 a2 && sequal a1 a3 -> Some (axiom_desc "Idempotent" <@fun x -> x ||| x = x @>)
        | _ -> None

    /// Axiom of the excluded middle.
    let (|ExcludedMiddle|_|) =
        function
        | Or(a1, Not(a2)), Bool true when sequal a1 a2 -> Some (axiom_desc "Excluded Middle" <@fun x -> x ||| not x @>)
        | _ -> None

    /// Golden rule axiom.
    let (|GoldenRule|_|) =
        function
        | And(p1, q1), Equiv(Equiv(p2, q2), Or(p3, q3)) when sequal p1 p2 && sequal p1 p3 && sequal q1 q2 && sequal q2 q3 -> 
                                                                Some (axiom_desc "Golden Rule" <@fun x y -> x |&| y = (x = y) = (x ||| y) @>)
        | _ -> None

    /// Implication and consequence axioms.
    let (|Implication|_|) =
        function
        | Implies(a1, a2), Equiv(Or(a3, a4), a5) when sequal a1 a3 && sequal a2 a4 && sequal a4 a5 -> 
                                                                Some (axiom_desc "Implication" <@fun x y-> (x ==> y) = ((x ||| y) = y)@>)
        | Conseq(a1, a2), Implies(a3, a4) when sequal2 a1 a2 a4 a3 -> 
                                                                Some (axiom_desc "Implication" <@fun x y -> (x <== y) = (y ==> x) @>)
        | _ -> None

    let (|LogicalAxioms|_|) =
        function
        | SEqual x
        | Symmetry x
        | Assoc x
        | Distrib x
        | Identity x
        | Duality x
        | Idempotent x
        | ExcludedMiddle x
        | GoldenRule x 
        | Implication x-> Some x
        | _ -> None

    let (|SymmLogicalAxioms|_|) =
        function
        | Symm(A, B) -> match (A, B) with | LogicalAxioms x -> Some x | _ -> None

    let logical_axioms =
        function
        | LogicalAxioms x
        | Conj(LogicalAxioms x) 
        | SymmLogicalAxioms x -> Some x
        | _ -> None
    
    (* Inference rules *) 
    
    /// Reduce logical constants.
    let rec reduce_constants  =
        function
        | Equiv(Bool l, Bool r) -> Expr.Value((l = r))
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
        | Equiv(Equiv(Equiv(And(p1, q1), p2), q2), Or(p3, q3))  -> <@@ true @@>
        | expr -> traverse expr golden_rule