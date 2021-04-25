namespace Sylvester

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Patterns
open Descriptions

/// Theory of algebraic operations on a ring of integers with binary operations (+) and (*), identities 0 and 1, 
/// and unary inverse operation (-).
module IntegerAlgebra =      
    /// Print Unicode logical operator symbols
    //let print_integer_algebra_operators (s:string) = 
    //    s.Replace("*", "\u22C5")
    let desc = axiom_desc "Integer Algebra"
    
    (* Axioms *)
    let integer_algebra_axioms =
        function                    
        | Assoc <@(=)@> <@ (+) @> x
        | Assoc <@(=)@> <@ (*) @> x
        | Identity <@(=)@> <@ (+) @> <@ 0 @> x 
        | Identity <@(=)@> <@ (*) @> <@ 1 @> x
        | Inverse <@(=)@> <@ (+) @> <@ (~-) @> <@ 0 @> x
        | Commute <@(=)@> <@ (+) @> x
        | Commute <@(=)@> <@ (*) @> x
        | Distrib <@(=)@> <@ (*) @> <@ (+) @> x 
        | LeftCancel <@ (*) @> x 
        | LeftCancel <@ (+) @> x
        | BinaryOpDefR <@(=)@> <@ (-) @> <@ (+) @> <@ (~-) @> x -> Some (desc x) 
        | _ -> None
