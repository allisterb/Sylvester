namespace Sylvester.CAS

open FSharp.Quotations

open Sylvester
open Maxima

open MathNet.Symbolics

module Algebra =
    let private send s = Maxima.send' s

    let algb_expand (expr:Expr<'t>)=
        sprintf "expand(%s);" (sprint' expr) 
        |> send 
        |> Result.mapError(fun e -> e.Message)
        |> Result.bind(fun o -> Infix.parse o)
        |> Result.map(fun e -> e|> (MathNetExpr.toQuotation<'t> (get_vars expr)))
        |> function
        | Ok s -> s
        | Error e -> failwithf "Error executing Maxima expand command: %s.\n. Session output:%s." e (Maxima.last_output())

    let algb_simplify (expr:Expr<'t>)=
        sprintf "simplify(%s);" (sprint' expr) 
        |> send 
        |> Result.mapError(fun e -> e.Message)
        |> Result.bind(fun o -> Infix.parse o)
        |> Result.map(fun e -> e|> (MathNetExpr.toQuotation<'t> (get_vars expr)))
        |> function
        | Ok s -> s
        | Error e -> failwithf "Error executing Maxima simplify command: %s.\n. Session output:%s." e (Maxima.last_output())

    let ratexpand (expr:Expr<'t>) =
        sprintf "ratexpand(%s);" (sprint' expr) 
        |> send 
        |> Result.mapError(fun e -> e.Message)
        |> Result.bind(fun o -> Infix.parse o)
        |> Result.map(fun e -> MathNetExpr.toQuotation<'t> (get_vars expr) e)
        |> function
        | Ok s -> s
        | Error e -> failwithf "Error executing Maxima ratexpand command: %s" e

    let partfrac (expr:Expr<'t>) (frac:Expr<'t>)=
        sprintf "partfrac(%s, %s);" (sprint' expr) (sprint' frac) 
        |> send 
        |> Result.mapError(fun e -> e.Message)
        |> Result.bind(fun o -> Infix.parse o)
        |> Result.map(fun e -> MathNetExpr.toQuotation<'t> (get_vars expr) e)
        |> function
        | Ok s -> s
        | Error e -> failwithf "Error executing Maxima partfrac command: %s" e

    let solve (v:Expr<'t>) (system:Expr<bool list>) =
        sprintf "solve(%s, %s);" (sprint' system) (sprint' v) 
        |> send 
        |> Result.mapError(fun e -> e.Message)
        |> Result.bind(fun o -> Infix.parseList (o.Split('=').[1]))
        |> Result.map(fun e -> e.[0] |> (MathNetExpr.toQuotation<'t> (get_vars system))) 
        |> function
        | Ok s -> s
        | Error e -> failwithf "Error executing Maxima solve command: %s.\n. Session output:%s." e (Maxima.defaultInt.Value.ConsoleSession.Last10Output)

    let solve_as_func_of (x:Expr<'b>) (v:Expr<'a>) (system:Expr<bool list>) = system |> solve v |> as_func_of x

    let solve_as_real_func_of (x:Expr<'b>) (v:Expr<'a>) (system:Expr<bool list>) = (solve_as_func_of x v system) >> real