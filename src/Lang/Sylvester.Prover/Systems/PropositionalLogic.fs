namespace Sylph

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.ExprShape

open Sylvester
open FormulaPatterns

module PropositionalLogic =
    let (|Equals|_|) =
        function
        | ShapeLambda(lv, lbody), ShapeLambda(rv, rbody) when vequal lv rv && sequal lbody rbody -> Some true
        | _ -> Some false
