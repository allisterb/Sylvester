namespace Sylvester.CAS

open FSharp.Quotations

open Sylvester
open Maxima

open MathNet.Symbolics

module Analysis =
    let private send (expr:Expr<'t>) cmd = 
        Maxima.send' cmd
        |> Result.mapError(fun e -> e.Message)
        |> Result.bind(fun o -> Infix.parse o)
        |> Result.map(fun e -> MathNetExpr.toQuotation'<'t> (get_vars expr) e)
        |> function
        | Ok s -> s
        | Error e -> failwithf "Error executing Maxima command %s: %s" cmd e

    let kill (v:Var) =
        match Maxima.send' <| sprintf "kill(%s);" v.Name with
        | Ok r -> if r.Trim() <> "done" then failwithf "Could not kill symbol %A. Maxima returned %s." v r
        | Error e -> failwithf "Could not kill symbol %A. Maxima returned %s." v e.Message

    let declare_constant (v:Var) =
        match Maxima.send' <| sprintf "declare(%s, constant);" v.Name with
        | Ok r -> if r.Trim() <> "done" then failwithf "Could not declare symbol %A as constant. Maxima returned %s." v r
        | Error e -> failwithf "Could not declare symbol %A as constant. Maxima returned %s." v e.Message

    let declare_constants v = List.iter declare_constant v

    let limit expr x v =
        send expr <| sprintf "limit(%s, %s, %s);" (sprint' expr) (sprint' x) (sprint' v)
                
    let limit_right expr x v =
        send expr <| sprintf "limit(%s, %s, %s, plus);" (sprint' expr) (sprint' x) (sprint' v)

    let limit_left expr x v =
        send expr <| sprintf "limit(%s, %s, %s, minus);" (sprint' expr) (sprint' x) (sprint' v)

    let diff expr x (constants:Var list) n =
        do constants |> List.iter(fun c -> declare_constant c)
        let r = send expr  <| sprintf "diff(%s, %s, %i);" (sprint' expr) (sprint' x) n
        do constants |> List.iter(fun c -> kill c)
        r