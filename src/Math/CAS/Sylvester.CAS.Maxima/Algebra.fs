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
           match send' <| sprintf "assume(%s > 0);" (sprint x) with
           | Ok r -> if r.Trim() <> (sprintf "[%s > 0]" (sprint x)) && r.Trim() <> "[redundant]" then failwithf "Could not make assumption. Maxima returned %s." r
           | Error e -> failwithf "Could not make assumption. Maxima returned %s." e.Message

    let algexpand (expr:Expr<'t>) = sprintf "expand(%s);" (sprint expr) |> sendCmd<'t> (get_vars expr)

    let ratexpand (expr:Expr<'t>) = sprintf "ratexpand(%s);" (sprint expr) |> sendCmd<'t> (get_vars expr)
    
    let ratsimp (expr:Expr<'t>) = sprintf "ratsimp(%s);" (sprint expr) |> sendCmd<'t> (get_vars expr)
    
    let factor (expr:Expr<'t>) = sprintf "factor(%s);" (sprint expr) |> sendCmd<'t> (get_vars expr)

    let factor_for (p:Expr<'t>) (expr:Expr<'t>) = sprintf "factor(%s, %s);" (sprint expr) (sprint p) |> sendCmd<'t> (get_vars expr)

    let partfrac_of (frac:Expr<'t>) (expr:Expr<'t>) = sprintf "partfrac(%s, %s);" (sprint expr) (sprint frac) |> sendCmd<'t> (get_vars expr)

    let solve_for (options:'a) (v:Expr<'t> list) (system:Expr<bool> list) =
        do if get_prop_else<bool> "posvars" false options  then system |> List.collect get_vars |> List.distinct |> List.map exprvar<real> |> List.iter assume_pos

        sprintf "solve(%s, %s);" (system |> sprintl) ("[" + (v |> List.collect get_vars |> List.distinct |> List.map (fun v -> sanitize_symbol v.Name) |> List.reduce(fun v1 v2 -> v1 + "," + v2)) + "]") 
        |> send 
        |> Result.mapError(fun e -> e.Message)
        |> Result.bind(fun o -> if o = "" then Error "" else if o = "[]" then Ok [] else Infix.parseEqnList o)
        |> Result.map(fun e -> e |> List.map (fun (i,_e) -> MathNetExpr.toQuotation<'t> (get_varsl system) i,MathNetExpr.toQuotation<'t> (get_varsl system) _e)) 
        |> Result.map(fun e -> e |> List.map (fun (l,r) -> <@ %l = %r @>))
        |> function
        | Ok s -> s
        | Error "" -> []
        | Error e -> failwithf "Error executing Maxima solve command: %s.\n. Session output:%s." e (Maxima.defaultInt.Value.ConsoleSession.Last10Output)

    let eliminate (options:'a) (v:Expr<'t> list) (system:Expr<bool> list) =
        do if get_prop_else<bool> "posvars" false options  then system |> List.collect get_vars |> List.distinct |> List.map exprvar<real> |> List.iter assume_pos
        let vars = get_varsl system
        sprintf "eliminate(%s, %s);" (system |> sprintl) ("[" + (v |> List.collect get_vars |> List.distinct |> List.map (fun v -> sanitize_symbol v.Name) |> List.reduce(fun v1 v2 -> v1 + "," + v2)) + "]") 
        |> send 
        |> Result.mapError(fun e -> e.Message)
        |> Result.bind(fun o -> if o = "" then Error "" else if o = "[]" then Ok [] else Infix.parseList o)
        |> Result.map(fun e -> e |> List.map (MathNetExpr.toQuotation<'t> vars)) 
        
        |> function
        | Ok s -> s
        | Error "" -> []
        | Error e when e.Contains "last: empty argument" -> []
        | Error e -> failwithf "Error executing Maxima eliminate command: %s.\n. Session output:%s." e (Maxima.defaultInt.Value.ConsoleSession.Last10Output)

    let collectterms (term:Expr<'t>) (expr:Expr<'t>) = sprintf "collectterms(%s, %s);" (sprint expr) (sprint term) |> sendCmd<'t> (get_vars expr)