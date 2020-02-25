namespace Sylph

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.ExprShape

open Sylvester
open FormulaPatterns

module BooleanAlgebra =
    // x || y, y || x
    // x && y, y && x
    let (|Commute|_|) =
        function
        | Lambda(v1, OrElse(a1, a2)), Lambda(v2, OrElse(b1, b2)) when vequal v1 v2 && sequal2 a1 a2 b2 b1 -> Some true
        | Lambda(v1, AndAlso(a1, a2)), Lambda(v2, AndAlso(b1, b2)) when vequal v1 v2 && sequal2 a1 a2 b2 b1 -> Some true          
        | _ -> None

    // x || y || z, x || (y || z)
    // x && y && z, x && (y && z)
    let (|Assoc|_|) =
        function
        | Lambda(v1, OrElse(OrElse(a1, a2), a3)), Lambda(v2, OrElse(b1, OrElse(b2, b3))) when vequal v1 v2 && sequal3 a1 a2 a3 b1 b2 b3 -> Some true        
        | Lambda(v1, OrElse(a1, OrElse(a2, a3))), Lambda(v2, OrElse(OrElse(b1, b2), b3)) when vequal v1 v2 && sequal3 a1 a2 a3 b1 b2 b3 -> Some true
        | Lambda(v1, AndAlso(AndAlso(a1, a2), a3)), Lambda(v2, AndAlso(b1, AndAlso(b2, b3))) when vequal v1 v2 && sequal3 a1 a2 a3 b1 b2 b3 -> Some true
        | Lambda(v1, AndAlso(a1, AndAlso(a2, a3))), Lambda(v2, AndAlso(AndAlso(b1, b2), b3)) when vequal v1 v2 && sequal3 a1 a2 a3 b1 b2 b3-> Some true

        | _ -> None

    // x && (y || z), x && y || x && z
    let (|Distrib|_|) =
        function
        | Lambda(v1, OrElse(AndAlso(a1, b1), AndAlso(a2, b2))), Lambda(v2, AndAlso(a3, OrElse(b3, b4))) when vequal v1 v2 && (sequal a1 a2) && (sequal a1 a3) && sequal2 b1 b2 b3 b4 -> Some true
        | _ -> None

    let (|OrIdentity|_|) = 
        function
        | Lambda(v1, a1), Lambda(v2, OrElse(a2, Bool false)) when vequal v1 v2 && sequal a1 a2 -> Some true
          
        | _ -> None

    let (|AndIdentity|_|) = 
        function
        | Lambda(v1, a1), Lambda(v2, AndAlso(a2, Bool true)) when vequal v1 v2 && sequal a1 a2 -> Some true        
        | Lambda(v1, AndAlso(a1, Bool true)), Lambda(v2, a2) when vequal v1 v2 && sequal a1 a2 -> Some true
        | _ -> None
