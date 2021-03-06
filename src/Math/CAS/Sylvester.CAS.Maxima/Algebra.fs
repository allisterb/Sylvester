﻿namespace Sylvester.CAS

open FSharp.Quotations

open Sylvester
open Maxima

open MathNet.Symbolics

module Algebra =
    let private send s = Maxima.send' s
    
    let partfrac (expr:Expr<'t>) (frac:Expr<'t>)=
        sprintf "partfrac(%s, %s);" (sprint' expr) (sprint' frac) 
        |> send 
        |> Result.mapError(fun e -> e.Message)
        |> Result.bind(fun o -> Infix.parse o)
        |> Result.map(fun e -> MathNetExpr.toQuotation'<'t> (get_vars expr) e)
        |> function
        | Ok s -> s
        | Error e -> failwithf "Error executing Maxima partfrac command: %s" e