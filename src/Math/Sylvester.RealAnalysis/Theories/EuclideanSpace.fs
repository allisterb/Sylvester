namespace Sylvester

open System
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Patterns
open Descriptions

open Vector
/// Theory of a Euclidean vector space over the field of real numbers
module EuclideanSpace =      
    let desc = axiom_desc "Euclidean Space"
    
    (* Axioms *)
    let eculidean_space_axioms =
        function                            
        | Assoc <@(=)@> (<@ (+) @> :Expr<Vec<_>->Vec<_>->Vec<_>>) x
        | Commute <@(=)@> (<@ (+) @> :Expr<Vec<_>->Vec<_>->Vec<_>>) x
        | Identity <@(=)@> (<@ (+) @> :Expr<Vec<_>->Vec<_>->Vec<_>>) <@ Vec<_>.Zero @> x 
        | Inverse <@(=)@> (<@ (+) @> :Expr<Vec<_>->Vec<_>->Vec<_>>) <@ vsmul -1.0 @> <@ Vec<_>.Zero @> x
        | LeftCancelNonZero (<@ (+) @> :Expr<Vec<_>->Vec<_>->Vec<_>>) <@ Vec<_>.Zero @> x -> Some (desc x)
        | Exists(_, a::[], Bool true, (Equals(Add(Var _, Var a'), Value(v, t)))) when vequal a a' && t = typeof<Vec<_>> && (v :?> Vec<_>) = Vec<_>.Zero -> Some (desc (pattern_desc' "Additive Inverse"))
        | Commute' <@(=)@> (<@ (*) @> :Expr<real->Vec<_>->Vec<_>>) x 
        | Distrib' <@(=)@> (<@ (*) @> :Expr<real->Vec<_>->Vec<_>>) (<@ (+) @> :Expr<Vec<_>->Vec<_>->Vec<_>>) x  -> Some (desc x)

        | _ -> None
    
