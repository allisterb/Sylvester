namespace Sylvester.CAS

open FSharp.Quotations

open Sylvester
open Maxima

open MathNet.Symbolics

module LinearAlgebra =
    let private get_vars_m(expr:Expr<_>[][]) = expr |> Array.map (Array.map(get_vars >> List.toArray)) |> Array.concat |> Array.concat |> Array.toList

    let private send<'t> vars cmd = 
        Maxima.send' cmd
        |> Result.mapError(fun e -> e.Message)
        |> Result.bind(fun o -> Infix.parse o)
        |> Result.map(fun e -> MathNetExpr.toQuotation<'t> vars e)
        |> function
        | Ok s -> s
        | Error e -> failwithf "Error executing Maxima command %s: %s" cmd e

    let private send_m (expr:Expr<_>[][]) cmd = 
        Maxima.send' cmd
        |> Result.mapError(fun e -> e.Message)
        |> Result.bind(fun o -> Infix.parse o)
        |> Result.map(fun e -> MathNetExpr.toQuotation<'t> (get_vars_m expr) e)
        |> function
        | Ok s -> s
        | Error e -> failwithf "Error executing Maxima command %s: %s" cmd e

    let charpoly (v:Expr<'t>) (expr:Expr<'t>[][])  =
        let vars = get_vars_m expr @ [get_var v]
        sprintf "charpoly(%s,%s)" (sprintm expr) (sprinte v) |> send<'t> vars

