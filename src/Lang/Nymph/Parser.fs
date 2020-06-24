namespace Sylvester.Nymph

open FSharp.Quotations

open Sylvester

module Parser =
    let parse<'t> = ExprParser.parse<'t>

