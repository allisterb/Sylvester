namespace Sylvester.CAS

open FSharp.Quotations

open Sylvester
open Maxima

open MathNet.Symbolics

module Algebra =
    let private send s = Maxima.send' s

    let private sendCmd<'t> vars cmd = 
        cmd
        |> send
        |> Result.mapError(fun e -> sprintf "Error executing Maxima command %s: %s. Maxima session output: %s" cmd e.Message (Maxima.last_output 10))
        |> Result.bind(fun o -> MathNet.Symbolics.Infix.parse (o.Replace("%", "")))
        |> Result.map(fun e -> MathNetExpr.toQuotation<'t> vars e)
        |> Result.mapError(fun e -> sprintf "Error parsing output of Maxima command %s: %s. Maxima session output: %s" cmd e (Maxima.last_output 10))
        |> function
        | Ok s -> s
        | Error e -> failwith e
    
    let assume_pos(x:Expr<'a>)  =
           match send' <| sprintf "assume(%s > 0);" (sprinte x) with
           | Ok r -> if r.Trim() <> (sprintf "[%s > 0]" (sprinte x)) && r.Trim() <> "[redundant]" then failwithf "Could not make assumption. Maxima returned %s." r
           | Error e -> failwithf "Could not make assumption. Maxima returned %s." e.Message

    let algexpand (expr:Expr<'t>) = sprintf "expand(%s);" (sprint expr) |> sendCmd<'t> (get_vars expr)

    let ratexpand (expr:Expr<'t>) = sprintf "ratexpand(%s);" (sprint expr) |> sendCmd<'t> (get_vars expr)
    
    let ratsimp (expr:Expr<'t>) = sprintf "ratsimp(%s);" (sprint expr) |> sendCmd<'t> (get_vars expr)
    
    let partfrac_of (frac:Expr<'t>) (expr:Expr<'t>) = sprintf "partfrac(%s, %s);" (sprint expr) (sprint frac) |> sendCmd<'t> (get_vars expr)

    let solve_for (v:Expr<'t>) (system:Expr<bool> list) =
        sprintf "solve(%s, %s);" (system |> sprintl) (sprint v) 
        |> send 
        |> Result.mapError(fun e -> e.Message)
        |> Result.bind(fun o -> if o = "" then Error "" else if o = "[]" then Ok [] else Infix.parseList o)
        |> Result.map(fun e -> e |> List.map (MathNetExpr.toQuotation<'t> (get_varsl system))) 
        |> function
        | Ok s -> s
        | Error "" -> []
        | Error e -> failwithf "Error executing Maxima solve command: %s.\n. Session output:%s." e (Maxima.defaultInt.Value.ConsoleSession.Last10Output)

    let solve_for_pos_vars (v:Expr<'t>) (e:Expr<bool> list) =
        e |> get_varsl |> List.map exprvar<real> |> List.iter assume_pos
        sprintf "solve(%s, %s);" (sprintl e) (sprint v) 
        |> send 
        |> Result.mapError(fun e -> e.Message)
        |> Result.bind(fun o -> if o = "" then Error ""  else if o = "[]" then Ok [] else Infix.parseList o)
        |> Result.map(fun exp -> exp |> List.map (MathNetExpr.toQuotation<'t> (get_varsl e))) 
        |> function
        | Ok s -> s
        | Error "" -> []
        | Error e -> failwithf "Error executing Maxima solve command: %s.\n. Session output:%s." e (Maxima.defaultInt.Value.ConsoleSession.Last10Output)

    let solve_for_n (options:'a) (v:Expr<'t> list) (system:Expr<bool> list) =
        do if get_prop_else<bool> "posvars" false options  then v |> List.iter assume_pos

        sprintf "solve(%s, %s);" (system |> sprintl) ("[" + (v |> List.collect get_vars |> List.distinct |> List.map (fun v -> v.ToString()) |> List.reduce(fun v1 v2 -> v1 + "," + v2)) + "]") 
        |> send 
        |> Result.mapError(fun e -> e.Message)
        |> Result.bind(fun o -> if o = "" then Error "" else if o = "[]" then Ok [] else Infix.parseList o)
        |> Result.map(fun e -> e |> List.map (MathNetExpr.toQuotation<'t> (get_varsl system))) 
        |> function
        | Ok s -> s
        | Error "" -> []
        | Error e -> failwithf "Error executing Maxima solve command: %s.\n. Session output:%s." e (Maxima.defaultInt.Value.ConsoleSession.Last10Output)

    let solve_as_real_eqn_for (v:Expr<real>) (expr:Expr<real>) = solve_for v [ <@ %expr = 0.0 @> ]

    let solve_for_as_func_of (x:Expr<'b>) (v:Expr<'a>) (system:Expr<bool> list) = system |> solve_for v |> List.head |> as_func_of x

    let solve_for2 (x:Expr<'t>) (y:Expr<'t>) (system:Expr<bool> list) =
        sprintf "solve(%s, [%s, %s]);" (sprintl system) (sprint x) (sprint y) 
        |> send 
        |> Result.mapError(fun e -> e.Message)
        |> Result.bind(fun e -> if e = "" then Error "" else Ok e)
        |> Result.map (fun e -> e.TrimStart('[').TrimEnd(']').Split(',') |> Array.map(fun v -> v.Split('=').[1])|> Array.reduce(fun r1 r2 -> r1 + "," + r2))
        |> Result.bind(fun r -> "[" + r + "]" |> MathNet.Symbolics.Infix.parseList)
        |> Result.map(fun e -> e |> List.map (MathNetExpr.toQuotation<'t> (system |> List.collect get_vars))) 
        |> function
        | Ok s -> s
        | Error "" -> []
        | Error e -> failwithf "Error executing Maxima solve command: %s.\n. Session output:%s." e (Maxima.last_output 10)

    //let solve_for_as_func_of2 (x:Expr<'b>)(y:Expr<'c>) (v:Expr<'a>) (system:Expr<bool list>) = system |> solve_for v |> Option.get |> as_func_of2 x y