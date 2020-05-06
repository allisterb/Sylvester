﻿namespace Sylvester

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Descriptions

/// Logical operators for formulas.
[<AutoOpen>]
module Operators =
    [<Unicode("\u2227")>]
    let (|&|) (l:bool) (r:bool) = l && r
    [<Unicode("\u2228")>]
    let (|||) (l:bool) (r:bool) = l || r
    let (==>) (l:bool) (r:bool) = (not l) || r
    let (<==) (l:bool) (r:bool) = r ==> l
 
/// Patterns used in axioms
module Patterns =   
    let (|Equals|_|) = 
         function
         | SpecificCall <@@ (=) @@> (None,_,l::r::[]) when l.Type = r.Type -> Some(l, r)
         | _ -> None
   
    let (|Not|_|) =
        function
        | SpecificCall <@@ not @@> (None,_,l::[]) -> Some l
        | _ -> None

    let (|NotEquals|_|) =
         function
         | SpecificCall <@@ (<>) @@> (None,_,l::r::[]) -> Some (l, r)
         | _ -> None

    let (|And|_|)  =
        function
        | SpecificCall <@@ (|&|) @@> (None,_,l::r::[]) -> Some (l, r)
        | _ -> None

    let (|Or|_|) =
        function
        | SpecificCall <@@ (|||) @@> (None,_,l::r::[]) -> Some (l, r)
        | _ -> None

    let (|Implies|_|) =
        function
        | SpecificCall <@@ (==>) @@> (None,_,l::r::[]) -> Some (l, r)
        | _ -> None

    let (|Conseq|_|) =
        function
        | SpecificCall <@@ (<==) @@> (None,_,l::r::[]) -> Some (l, r)
        | _ -> None

    let (|Add|_|) =
        function
        | SpecificCall <@@ (+) @@> (None,_,l::r::[]) -> Some(l, r)
        | _ -> None

    let (|Subtract|_|) =
        function
        | SpecificCall <@@ (-) @@> (None,_,l::r::[]) -> Some(l, r)
        | _ -> None
    
    let (|Negate|_|) =
        function
        | SpecificCall <@@ (~-) @@> (None,_,r::[]) -> Some r
        | _ -> None

    let (|Multiply|_|) =
        function
        | SpecificCall <@@ (*) @@> (None,_,l::r::[]) -> Some(l, r)
        | _ -> None
        
    let (|Range|_|) =
        function
        | SpecificCall <@@ (..) @@> (None,_,l::r::[]) -> Some(l,r)
        | _ -> None

    let (|Sequence|_|) =
        function
        | Call(None, method, Range(l, r)::[]) when method.Name = "CreateSequence" -> Some (l, r)
        | _ -> None

    let (|Sum|_|)  =
        function
        | Call(None, method, Sequence(l, r)::[]) when method.Name = "Sum" -> Some (l, r)
        | _ -> None

    let (|Binary|_|) (op:Expr<'t->'t->'u>) =
        function
        | SpecificCall op (None,_,l::r::[]) when l.Type = typeof<'t> && r.Type = typeof<'t> -> Some (l,r)
        | _ -> None

    let (|Unary|_|) (op:Expr<'t->'u>)=
        function
        | SpecificCall op (None,_,r::[]) when r.Type = typeof<'t> -> Some r
        | _ -> None

    let (|BinaryCall|_|)  =
        function
        | Call(_,_,l::r::[]) when l.Type = r.Type -> Some (l,r)
        | _ -> None

    let (|UnaryCall|_|)  =
        function
        | Call(_,_,r::[]) -> Some r
        | _ -> None

    /// Main axiom of Sylph's symbolic equality. A and B are equal if they are: 
    /// * Syntactically valid F# expressions
    /// * Decomposed to the same sequence of symbols i.e. strings.
    /// Since we are only concerned with string equality this law encompasses all 4 of the equational logic laws of equality:
    /// Symmetry, reflexivity, transitivity, and Leibniz's rule: A = B => S(A) = S(B)
    let (|SEqual|_|) =
        function
        | Equals(A, B) when sequal A B -> pattern_desc "Symbolic Equality" <@ fun x y -> x = y @> |> Some
        | _ -> None

    /// (x = x)
    let (|Reflex|_|) (op:Expr<'t->'t->bool>) =
        function
        | Binary op (a1, a2) when sequal a1 a2 -> 
            pattern_desc (sprintf "Reflexivity of %s" (src op)) <@ fun x -> (%op) x x @> |> Some
        | _ -> None

    /// (x + y) + z = x + (y + z)
    let (|Assoc|_|) (eq:Expr<'t->'t->bool>)  (op:Expr<'t->'t->'t>) =
        function
        | Binary eq (Binary op (Binary op (a1, a2), a3), Binary op (b1, Binary op (b2, b3))) when sequal3 a1 a2 a3 b1 b2 b3 -> 
            pattern_desc "Associativity" <@ fun x y z -> (%eq) ((%op) ((%op) x y) z) ((%op) x ((%op) y z)) @> |> Some
        | _ -> None

    /// (x = y = y) = x
    let (|Symm|_|) (op:Expr<'t->'t->'t>)   =
        function
        | Binary op (Binary op (Binary op (a1, a2), a3), a4)  when sequal2 a1 a2 a4 a3-> pattern_desc "Symmetry" <@ fun x y  -> (%op) (((%op) x y)) ((%op) y x) @> |> Some
        | _ -> None 

    /// (x + y) = (y + x)
    let (|Commute|_|) (eq:Expr<'t->'t->bool>)  (op: Expr<'t->'t->'t>) =
        function
        | Binary eq (Binary op (a1, a2), Binary op (b1, b2)) when sequal2 a1 a2 b2 b1 -> pattern_desc "Commutativity" <@fun (x:'t) (y:'t) -> (%eq) ((%op) x y) ((%op) y x) @> |> Some   
        | _ -> None

    /// x + 0 = x
    let (|Identity|_|) (eq:Expr<'t->'t->bool>)  (op: Expr<'t->'t->'t>) (zero:Expr<'t>)   = 
        function
        | Binary eq (Binary op (a1, z), a2) when sequal a1 a2 && sequal zero z -> pattern_desc (sprintf "Identity of %s" (src op)) <@ fun (x:'t) -> (%eq) ((%op) x (%zero)) (%zero) @> |> Some
        | _ -> None

    /// x * (y + z) = x * y + x * z
    let (|Distrib|_|) (eq:Expr<'t->'t->bool>)  (op1: Expr<'t->'t->'t>) (op2: Expr<'t->'t->'t>)  = 
        function
        | Binary eq (Binary op1 (a3, Binary op2 (b3, b4)), Binary op2 (Binary op1 (a1, b1), Binary op1 (a2, b2))) when (sequal a1 a2) && (sequal a1 a3) && sequal2 b1 b2 b3 b4 -> 
                pattern_desc "Distributivity" <@ fun x y z -> (%eq) ((%op1) x ((%op2) y z)) ((%op2) ((%op1) x y) ((%op1) x z)) @> |> Some
        | _ -> None

    ///  -(y + z) = -x  * - z
    let (|UnaryDistrib|_|) (eq:Expr<'t->'t->bool>)  (op1: Expr<'t->'t>) (op2: Expr<'t->'t->'t>)  = 
        function
        | Binary eq (Unary op1 (Binary op2 (a1, a2)), Binary op2 (Unary op1 a3, Unary op1 a4)) when sequal a1 a3 && sequal a2 a4 -> 
                pattern_desc "Distributivity" <@ fun x y -> (%eq) ((%op1) ((%op2) x y)) ((%op2) ((%op1) x ) ((%op1) y)) @> |> Some
        | _ -> None

    /// x + (-x) = zero
    let (|Inverse|_|) (eq:Expr<'t->'t->bool>)  (op: Expr<'t->'t->'t>) (inverse: Expr<'t -> 't>) (zero: Expr<'t>)   =
        function
        | Binary eq (Binary op (a1, Unary inverse (a2)), z)  when sequal a1 a2 && sequal zero z -> pattern_desc "Definition of Inverse" <@ fun (x:'t) -> (%eq) ((%op) x ((%inverse) x)) (%zero) @> |> Some
        | _ -> None

    /// x + x = x
    let (|Idempotency|_|) (eq:Expr<'t->'t->bool>)  (op: Expr<'t->'t->'t>) = 
        function
        | Binary eq (Binary op (a1, a2), a3) when sequal a1 a2 && sequal a1 a3 -> pattern_desc "Idempotency" <@ fun (x:'t) -> (%eq) ((%op) x x) x @> |> Some
        | _ -> None

    /// not (x = y) = not(x) <> not(y)
    let (|Duality|_|) (eq:Expr<'t->'t->bool>)  (op1: Expr<'t->'t->'t>) (op2: Expr<'t->'t->'t>) (inverse: Expr<'t->'t>) =
        function
        | Binary eq (Unary inverse (Binary op1 (a1, a2)), Binary op2 (Unary inverse b1, Unary inverse b2)) when sequal2 a1 a2 b2 b1 -> 
            pattern_desc "Duality" <@ fun x y -> (%eq) ((%inverse)((%op1) x y)) ((%op2) ((%inverse)(x)) ((%inverse)(y))) @> |> Some
        | _ -> None

    /// Define the LHS by the RHS
    let (|Def|_|) (eq:Expr<'t->'t->bool>) (l:Expr<'t>) (r:Expr<'t>) =
        function
        | Binary eq (a1, a2) when sequal2 a1 a2 l r -> pattern_desc (sprintf "Definition of %s" (src l)) <@ (%eq) %l %r @> |> Some
        | _ -> None

    /// Define a binary operator by another binary operator and a unary operator applied to the entire expression e.g p <> q = not (p = q).
    let (|BinaryOpDef|_|) (eq:Expr<'t->'t->bool>)  (op1:Expr<'t->'t->'t>) (op2:Expr<'t->'t->'t>) (op3:Expr<'t->'t>)=
        function
        | Binary eq (Binary op1 (a1, a2), Unary op3 (Binary op2 (a3, a4))) when sequal2 a1 a2 a3 a4 -> pattern_desc (sprintf "Definition of %s" (src op1)) <@ fun x y -> (%eq) ((%op1) x y) ((%op2) x y) @> |> Some
        | _ -> None

    //. Define a binary operator by another binary operator and a unary operator applied to the left of the expression
    let (|BinaryOpDefL|_|) (eq:Expr<'t->'t->bool>)  (op1:Expr<'t->'t->'t>) (op2:Expr<'t->'t->'t>) (op3:Expr<'t->'t>)=
        function
        | Binary eq (Binary op1 (a1, a2), (Binary op2 (Unary op3 a3, a4))) when sequal2 a1 a2 a3 a4 -> pattern_desc (sprintf "Definition of %s" (src op1)) <@ fun x y -> (%eq) ((%op1) x y) ((%op2) x y) @> |> Some
        | _ -> None

    let (|BinaryOpDefR|_|) (eq:Expr<'t->'t->bool>)  (op1:Expr<'t->'t->'t>) (op2:Expr<'t->'t->'t>) (op3:Expr<'t->'t>)=
        function
        | Binary eq (Binary op1 (a1, a2), (Binary op2 (a3, Unary op3 a4))) when sequal2 a1 a2 a3 a4 -> pattern_desc (sprintf "Definition of %s" (src op1)) <@ fun x y -> (%eq) ((%op1) x y) ((%op2) x y) @> |> Some
        | _ -> None

    let (|LeftCancel|_|) (op:Expr<'t->'t->'t>)  =
        function
        | Equals (Equals(Binary op (a1, b), Binary op (a2, c)), Equals(b1, c1)) when sequal a1 a1 && sequal b b1 && sequal c c1 
            -> pattern_desc "Left Cancellation" <@ fun a b c -> (((%op) a b = (%op) a c)) = ((b = c)) @> |> Some
        | _ -> None

    let (|RightCancel|_|) (op:Expr<'t->'t->'t>)  =
        function
        | Equals (Equals (Binary op (b, a1), Binary op (c, a2)), Equals (b1, c1)) when sequal a1 a1 && sequal b b1 && sequal c c1 
            -> pattern_desc "Right Cancellation" <@ fun a b c -> ((%op) b a = (%op) c a) = (b = c) @> |> Some
        | _ -> None

    let (|Value|_|) (v:'t) =
        function
        | Value(z, t) when (t = typeof<'t>) && ((z :?> 't) = v) -> Expr.Value(v) |> Some
        | _ -> None

[<AutoOpen>]
module Formula =  
    // Make Formula an alias for the reflected definition attribute
    type Formula = ReflectedDefinitionAttribute
   
    // Introduce variable names for formulas
    let var<'t> = Unchecked.defaultof<'t>
    let var2<'t> = Unchecked.defaultof<'t>, Unchecked.defaultof<'t>
    let var3<'t> = Unchecked.defaultof<'t>, Unchecked.defaultof<'t>, Unchecked.defaultof<'t>
    let var4<'t> = Unchecked.defaultof<'t>, Unchecked.defaultof<'t>, Unchecked.defaultof<'t>, Unchecked.defaultof<'t>