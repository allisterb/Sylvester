namespace Sylvester

open FSharp.Quotations

open Patterns
open Descriptions

/// Theory of algebraic operations on an integral domain of integers with binary operations (+) and (*) and (*) distributes over (+), 
/// identities 0 and 1, and unary inverse operation (-), where c <> 0  ==> (c * a = c * b = (a = b)).
module IntegerAlgebra =      
    (* Symbols *)
    do Symbols.BulitIn.Add(src <@ (*) @>, "\u22C5")
    
    let desc = axiom_desc "Integer Algebra"
    
    (* Axioms *)
    let integer_algebra_axioms =
        function                    
        | Assoc <@(=)@> <@ (+) @> x
        | Assoc <@(=)@> <@ (*) @> x
        | Commute <@(=)@> <@ (+) @> x
        | Commute <@(=)@> <@ (*) @> x
        | Identity <@(=)@> <@ (+) @> <@ 0 @> x 
        | Identity <@(=)@> <@ (*) @> <@ 1 @> x
        | Inverse <@(=)@> <@ (+) @> <@ (~-) @> <@ 0 @> x
        | Distrib <@(=)@> <@ (*) @> <@ (+) @> x  
        | LeftCancelNonZero <@ (+) @> <@ 0 @> x
        | BinaryOpDefR <@(=)@> <@ (-) @> <@ (+) @> <@ (~-) @> x -> Some (desc x) 
        | _ -> None
