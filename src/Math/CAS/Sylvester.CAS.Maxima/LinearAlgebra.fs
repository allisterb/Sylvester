namespace Sylvester.CAS

open FSharp.Quotations

open Sylvester
open Maxima

open MathNet.Symbolics

module LinearAlgebra =
    let private send (expr:Expr<'t>) cmd = 
        Maxima.send' cmd
        |> Result.mapError(fun e -> e.Message)
        |> Result.bind(fun o -> Infix.parse o)
        |> Result.map(fun e -> MathNetExpr.toQuotation<'t> (get_vars expr) e)
        |> function
        | Ok s -> s
        | Error e -> failwithf "Error executing Maxima command %s: %s" cmd e

    let rank expr x v =
        send expr <| sprintf "limit(%s, %s, %s);" (sprint' expr) (sprint' x) (sprint' v)