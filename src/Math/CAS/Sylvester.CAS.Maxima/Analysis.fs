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
        |> Result.mapError(fun e -> sprintf "Error executing Maxima command %s: %s. Maxima session output: %s" cmd e.Message (Maxima.last_output 10))
        |> Result.bind(fun o -> MathNet.Symbolics.Infix.parse (o.Replace("%", "")))
        |> Result.map(fun e -> MathNetExpr.toQuotation<'t> vars e)
        |> Result.mapError(fun e -> sprintf "Error parsing output of Maxima command %s: %s. Maxima session output: %s" cmd e (Maxima.last_output 10))
        |> function
        | Ok s -> s
        | Error e -> failwith e

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

    let sum x l u (expr:Expr<'t>) = 
        assume <@ %u > %l @>
        let r = sendCmd<'t> (get_vars expr) <| sprintf "sum(%s, %s, %s, %s);" (sprint expr) (sprint x) (sprint l) (sprint u)
        forget <@ %u > %l @>
        r
    
    let trigsimp (expr:Expr<'t>) = sprintf "trigsimp(%s);" (sprint expr) |> sendCmd<'t> (get_vars expr)

    let trigexpand (expr:Expr<'t>) = sprintf "trigexpand(%s);" (sprint expr) |> sendCmd<'t> (get_vars expr)

    let trigrat (expr:Expr<'t>) = sprintf "trigrat(%s);" (sprint expr) |> sendCmd<'t> (get_vars expr)

    let trigreduce (expr:Expr<'t>) = sprintf "trigreduce(%s);" (sprint expr) |> sendCmd<'t> (get_vars expr)

    let limit x v (expr:Expr<'t>) =
        sendCmd<'t> (get_vars expr) <| sprintf "limit(%s, %s, %s);" (sprint expr) (sprint x) (sprint v)
                
    let limit_right x v (expr:Expr<'t>) = sendCmd<'t> (get_vars expr) <| sprintf "limit(%s, %s, %s, plus);" (sprint expr) (sprint x) (sprint v)

    let limit_left x v (expr:Expr<'t>) = sendCmd<'t> (get_vars expr) <| sprintf "limit(%s, %s, %s, minus);" (sprint expr) (sprint x) (sprint v)

    let diffn n x (expr:Expr<'t>) = sendCmd<'t> (get_vars expr) <| sprintf "diff(%s, %s, %i);" (sprint expr) (sprint x) n
        
    let diff x (expr:Expr<'t>) = diffn 1 x expr
    
    let integrate x (expr:Expr<'t>) = sendCmd<'t> (get_vars expr @ get_vars x) <| sprintf "integrate(%s, %s);" (sprint expr) (sprint x)
    
    let definite_integral x l u (expr:Expr<'t>) = 
        let l' =
            match l with
            | NegInf -> "minf"
            | _ -> sprint l
        let u' =
            match u with
            | PosInf -> "inf"
            | _ -> sprint u

        do if l' <> "minf" && u' <> "inf" then assume <@ %u > %l @>
        let r = sendCmd<'t>(get_vars expr @ get_vars x @ get_vars l @ get_vars u) <| sprintf "integrate(%s, %s, %s, %s);" (sprint expr) (sprint x) l' u'
        do if l' <> "minf" && u' <> "inf" then forget <@ %u > %l @>
        r

    let taylor_series(expr:Expr<real>) (x:Expr<'t>) (a:Expr<'t>) (n:int) = sprintf "taylor(%s, %s, %s, %A);" (sprint expr) (sprint x) (sprint a) n |> sendCmd<'t> (get_vars expr)