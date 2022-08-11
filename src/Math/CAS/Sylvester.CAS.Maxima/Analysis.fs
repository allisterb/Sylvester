namespace Sylvester.CAS

open FSharp.Quotations
open FSharp.Quotations.Patterns

open Sylvester
open Maxima

module Analysis =
    
    let private send s = Maxima.send' s
    
    let private sendCmd<'t> vars cmd = 
        cmd
        |> send
        |> Result.mapError(fun e -> e.Message)
        |> Result.bind(fun o -> MathNet.Symbolics.Infix.parse o)
        |> Result.map(fun e -> MathNetExpr.toQuotation<'t> vars e)
        |> function
        | Ok s -> s
        | Error e -> failwithf "Error executing Maxima command %s: %s. Maxima session output: %s" cmd e (Maxima.last_output 10)

    let kill (v:Var) =
        match send <| sprintf "kill(%s);" v.Name with
        | Ok r -> if r.Trim() <> "done" then failwithf "Could not kill symbol %A. Maxima returned %s." v r
        | Error e -> failwithf "Could not kill symbol %A. Maxima returned %s." v e.Message

    let declare_constant (v:Var) =
        match send <| sprintf "declare(%s, constant);" v.Name with
        | Ok r -> if r.Trim() <> "done" then failwithf "Could not declare symbol %A as constant. Maxima returned %s." v r
        | Error e -> failwithf "Could not declare symbol %A as constant. Maxima returned %s." v e.Message

    let assume (a:Expr<bool>) =
        let a' = sprinte a
        match send <| sprintf "assume((%s));" a'  with
        | Ok r -> if r.Trim() <> sprintf "[%s]" a' && r.Trim() <> "[redundant]" then failwithf "Could not add assumption %s. Unrecognized response: %s." a' r
        | Error e -> failwithf "Could not add assumption %s. Maxima returned: %s." a' (e.Message)

    let forget (a:Expr<bool>) =
        let a' = sprint a
        match send <| sprintf "forget(%s);" a'  with
        | Ok _ -> ()
        | Error e -> failwithf "Could not forget assumption %s. Maxima returned: %s." a' (e.Message)

    let declare_constants v = List.iter declare_constant v

    let sum (expr:Expr<'t>) x l u = 
        assume <@ %u > %l @>
        let r = sendCmd<'t> (get_vars expr) <| sprintf "sum(%s, %s, %s, %s);" (sprint expr) (sprint x) (sprint l) (sprint u)
        forget <@ %u > %l @>
        r
    
    let trigsimp (expr:Expr<'t>) = sprintf "trigsimp(%s);" (sprint expr) |> sendCmd<'t> (get_vars expr)

    let trigexpand (expr:Expr<'t>) = sprintf "trigexpand(%s);" (sprint expr) |> sendCmd<'t> (get_vars expr)

    let limit (expr:Expr<'t>) x v =
        sendCmd<'t> (get_vars expr) <| sprintf "limit(%s, %s, %s);" (sprint expr) (sprint x) (sprint v)
                
    let limit_right (expr:Expr<'t>) x v = sendCmd<'t> (get_vars expr) <| sprintf "limit(%s, %s, %s, plus);" (sprint expr) (sprint x) (sprint v)

    let limit_left (expr:Expr<'t>) x v = sendCmd<'t> (get_vars expr) <| sprintf "limit(%s, %s, %s, minus);" (sprint expr) (sprint x) (sprint v)

    let diff (expr:Expr<'t>) x n = sendCmd<'t> (get_vars expr) <| sprintf "diff(%s, %s, %i);" (sprint expr) (sprint x) n
        
    let integrate (expr:Expr<'t>) x = sendCmd<'t> (get_vars expr) <| sprintf "integrate(%s, %s);" (sprint expr) (sprint x)
    
    let definite_integral (expr:Expr<'t>) x l u = 
        let l' =
            match l with
            | NegInf -> "minf"
            | _ -> sprint l
        let u' =
            match u with
            | PosInf -> "inf"
            | _ -> sprint u

        do if l' <> "minf" && u' <> "inf" then assume <@ %u > %l @>
        let r = sendCmd<'t>(get_vars expr) <| sprintf "integrate(%s, %s, %s, %s);" (sprint expr) (sprint x) l' u'
        do if l' <> "minf" && u' <> "inf" then forget <@ %u > %l @>
        r

