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

    let partfrac_of (frac:Expr<'t>) (expr:Expr<'t>) =
        sprintf "partfrac(%s, %s);" (sprint' expr) (sprint' frac) 
        |> send 
        |> Result.mapError(fun e -> e.Message)
        |> Result.bind(fun o -> Infix.parse o)
        |> Result.map(fun e -> MathNetExpr.toQuotation<'t> (get_vars expr) e)
        |> function
        | Ok s -> s
        | Error e -> failwithf "Error executing Maxima partfrac command: %s" e

    let solve_for (v:Expr<'t>) (system:Expr<bool list>) =
        sprintf "solve(%s, %s);" (sprint' system) (sprint' v) 
        |> send 
        |> Result.mapError(fun e -> e.Message)
        |> Result.bind(fun o -> if o = "" then Error "" else Infix.parseList (o.Split('=').[1]))
        |> Result.map(fun e -> e.[0] |> (MathNetExpr.toQuotation<'t> (get_vars system))) 
        |> function
        | Ok s -> Some s
        | Error "" -> None
        | Error e -> failwithf "Error executing Maxima solve command: %s.\n. Session output:%s." e (Maxima.defaultInt.Value.ConsoleSession.Last10Output)

    let solve_for_as_func_of (x:Expr<'b>) (v:Expr<'a>) (system:Expr<bool list>) = system |> solve_for v |> Option.get |> as_func_of x

    let solve_for2 (x:Expr<'t>) (y:Expr<'t>) (system:Expr<bool list>) =
        sprintf "solve(%s, [%s, %s]);" (sprint' system) (sprint' x) (sprint' y) 
        |> send 
        |> Result.mapError(fun e -> e.Message)
        |> Result.bind(fun e -> if e = "" then Error "" else Ok e)
        |> Result.map (fun e -> e.TrimStart('[').TrimEnd(']').Split(',') |> Array.map(fun v -> v.Split('=').[1])|> Array.reduce(fun r1 r2 -> r1 + "," + r2))
        |> Result.bind(fun r -> "[" + r + "]" |> MathNet.Symbolics.Infix.parseList)
        |> Result.map(fun e -> e |> List.map (MathNetExpr.toQuotation<'t> (get_vars system))) 
        |> function
        | Ok (r1::r2::[]) -> Some(r1, r2)
        | Error "" -> None
        | Ok r -> failwithf "Error executing Maxima solve command: received unexpected solution output: %A." r
        | Error e -> failwithf "Error executing Maxima solve command: %s.\n. Session output:%s." e (Maxima.defaultInt.Value.ConsoleSession.Last10Output)

    let solve_for_as_func_of2 (x:Expr<'b>)(y:Expr<'c>) (v:Expr<'a>) (system:Expr<bool list>) = system |> solve_for v |> Option.get |> as_func_of2 x y