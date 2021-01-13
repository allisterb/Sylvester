#load "Include.fsx"

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Sylph

open Sylvester
open FormulaPatterns



[<ReflectedDefinition;AutoOpen>]
module Formulae =
    let sum n = Seq.sum (seq {1..n})
    let sum1 n = Seq.sum (seq {1..n + 1})

let F1 = F sum

