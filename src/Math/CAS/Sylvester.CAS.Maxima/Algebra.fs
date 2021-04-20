namespace Sylvester.CAS

open FSharp.Quotations

open Sylvester
open Maxima

open MathNet.Symbolics

module Algebra =
    let private send s = 
        match send (Maxima.defaultInt.Value) s with
        | Success o -> Ok o
        | Failure e -> Error e

    let partfrac (expr:Expr<'t>) (frac:Expr<'t>)=
        sprintf "partfrac(%s, %s);" (print_formula expr) (print_formula frac) 
        |> send 
        |> Result.mapError(fun e -> e.Message)
        |> Result.bind(fun o -> Infix.parse o)
        |> Result.map(fun e -> MathNetExpr.toQuotation'<'t> (get_vars frac) e)
        |> function
        | Ok s -> expand'<'t, 't> <@ %%s:'t @>
        | Error e -> failwithf "Error executing Maxima command: %s." e
