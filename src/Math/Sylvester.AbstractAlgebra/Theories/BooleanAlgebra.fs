namespace Sylvester

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Sylvester
open Patterns
open Descriptions

/// Theory of Boolean algebra on a set closed under 2 binary operations that are associative, commutative, and idempotent,
/// with identity elements 0 and 1, and a unary inverse or complement operation.
module BooleanAlgebra =
    let desc = axiom_desc "Boolean Algebra" id
    
    (* Axioms *)

    let boolean_algebra_axioms (theoryName:string) (join: Expr<'t->'t->'t>) (meet: Expr<'t->'t->'t>) (zero: Expr<'t>) (one: Expr<'t>) (comp: Expr<'t->'t>) = 
        function
        | Assoc <@(=)@> join x
        | Assoc <@(=)@> meet x
                
        | Identity <@(=)@> join zero x
        | Identity <@(=)@> meet one x
                
        | Inverse <@(=)@> join comp zero x
        | Inverse <@(=)@> meet comp one x

        | Idempotency <@(=)@> join x
        | Idempotency <@(=)@> meet x

        | Commute <@(=)@> join x
        | Commute <@(=)@> meet x

        | Distrib <@(=)@> join meet x 
        | Distrib <@(=)@> meet join x -> desc x |> set_axiom_desc_theory theoryName |> Some
        | _ -> None

    (* Rules *)
    
    let rec reduce_idemp (join: Expr<'t->'t->'t>) (meet: Expr<'t->'t->'t>) (zero: Expr<'t>)  (one: Expr<'t>) (comp:Expr<'t -> 't>) =
        function
        | Binary join (a1, a2) when sequal a1 a2 -> <@@ %%a1 @@>
        | Binary meet (a1, a2) when sequal a1 a2 -> <@@ %%a1 @@> 
        | expr -> traverse expr (reduce_idemp join meet zero one comp)

    let rec reduce_ident (join: Expr<'t->'t->'t>) (meet: Expr<'t->'t->'t>) (zero: Expr<'t>)  (one: Expr<'t>) (comp:Expr<'t -> 't>) =
        function
        | Binary join (a1, Value zero _) -> <@@ %%a1 @@>
        | Binary meet (a1, Value one _) -> <@@ %%a1 @@>
        | expr -> traverse expr (reduce_ident join meet zero one comp)

    let rec reduce_comp (join: Expr<'t->'t->'t>) (meet: Expr<'t->'t->'t>) (zero: Expr<'t>) (one: Expr<'t>) (comp:Expr<'t -> 't>) =
        function
        | Binary join (a1, Unary comp a2) when sequal a1 a2 -> <@@ zero @@>
        | Binary join (a1, Unary comp a2) when sequal a1 a2 -> <@@ one @@>
        | expr -> traverse expr (reduce_comp join meet zero one comp)

    let rec right_assoc (join: Expr<'t->'t->'t>) (meet: Expr<'t->'t->'t>) =
        function
        | Binary join (Binary join (a1, a2), a3) -> <@@ (%join) %%a1  ((%join) %%a2 %%a3) @@>
        | Binary meet (Binary meet (a1, a2), a3) -> <@@ (%join) %%a1  ((%join) %%a2 %%a3) @@>
        | expr -> traverse expr (right_assoc join meet)

    let rec left_assoc (join: Expr<'t->'t->'t>) (meet: Expr<'t->'t->'t>) =
        function
        | Binary join (a1, Binary join (a2, a3)) -> <@@ (%join) ((%join) %%a1 %%a2) %%a3 @@>
        | Binary meet (a1, Binary meet (a2, a3)) -> <@@ (%meet) ((%meet) %%a1 %%a2) %%a3 @@>
        | expr -> traverse expr (left_assoc meet meet)

    let rec commute (join: Expr<'t->'t->'t>) (meet: Expr<'t->'t->'t>) =
        function
        | Binary join (a1, a2) -> <@@ (%join) %%a2 %%a1 @@>
        | Binary meet (a1, a2) -> <@@ (%meet) %%a2 %%a1 @@>
        | expr -> traverse expr (commute join meet)

    let rec distrib (join: Expr<'t->'t->'t>) (meet: Expr<'t->'t->'t>) =
        function
        // x * ( y + z) == x * y + x * z
        | Binary meet (a1, Binary join (a2, a3)) -> <@@ (%join) ((%meet) %%a1 %%a2)  ((%meet) %%a1 %%a3) @@> 
        // x + ( y * z) == x + y * x * z
        | Binary join (a1, Binary meet (a2, a3)) -> <@@ (%meet) ((%join) %%a1 %%a2)  ((%join) %%a1 %%a3) @@> 
        | expr -> traverse expr (distrib join meet)

    /// Idempotent operation in (expression) can be reduced.
    let ReduceIdemp join meet zero one comp = Admit("Idempotent operation in (expression) can be reduced", reduce_idemp join meet zero one comp)
    
    /// Identity in (expression) can be reduced.
    let ReduceIdent join meet zero one comp = Admit("Identity in (expression) can be reduced", reduce_ident join meet zero one comp)

    /// Complement operation in (expression) can be reduced.
    let ReduceComp join meet zero one comp = Admit("Complement operation in (expression) can be reduced", reduce_comp join meet zero one comp)
    
    /// Expression is left-associative.
    let LeftAssoc join meet = Admit("(expression) is left-associative", left_assoc join meet)
    
    /// Expression is right associative.
    let RightAssoc join meet = Admit("(expression) is right-associative", right_assoc join meet)

    /// Expression is commutative.
    let Commute join meet = Admit("(expression) is commutative", commute join meet)

    /// Expression is commutative.
    let Distrib join meet = Admit("(expression) is distributive", distrib join meet)

    type BooleanAlgebraTheory<'t when 't: equality>(theoryName: string, join: Expr<'t->'t->'t>, meet: Expr<'t->'t->'t>, zero: Expr<'t>, one: Expr<'t>, comp: Expr<'t->'t>, ?formulaPrinter:string->string) = 
        inherit Theory(boolean_algebra_axioms theoryName join meet zero one comp, [
            ReduceIdemp join meet zero one comp
            ReduceIdent join meet zero one comp
            ReduceComp join meet zero one comp
            LeftAssoc join meet
            RightAssoc join meet
            Commute join meet
            Distrib join meet
        ], defaultArg formulaPrinter id)