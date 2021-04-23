namespace Sylvester.CAS

open FSharp.Quotations

open Sylvester
open Maxima

open MathNet.Symbolics

type LimitType =
| Above
| Below
| Both
with 
    static member op_Explicit(l:LimitType) : string =
        match l with
        | Above -> "plus"
        | Below -> "minus"
        | Both -> ""

module Analysis =
    let private send s = Maxima.send' s

    let limit (expr:Expr<'t>) (x:Expr<'t>) (v:Expr<'t>) =
        sprintf "limit(%s, %s, %s);" (print_formula expr) (print_formula x) (print_formula v) 
        |> send 
        |> Result.mapError(fun e -> e.Message)
        |> Result.bind(fun o -> Infix.parse o)
        |> Result.map(fun e -> MathNetExpr.toQuotation'<'t> (get_vars expr) e)
        |> function
        | Ok s -> expand'<'t, 't> <@ %%s:'t @>
        | Error e -> failwithf "Error executing Maxima limit command: %s" e