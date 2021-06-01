namespace Sylvester.CAS

open FSharp.Quotations
open FSharp.Quotations.Patterns

open Sylvester
open Maxima

module Analysis =
    let private send (expr:Expr<'t>) cmd = 
        Maxima.send' cmd
        |> Result.mapError(fun e -> e.Message)
        |> Result.bind(fun o -> MathNet.Symbolics.Infix.parse o)
        |> Result.map(fun e -> MathNetExpr.toQuotation'<'t> (get_vars expr) e)
        |> function
        | Ok s -> s
        | Error e -> failwithf "Error executing Maxima command %s: %s. The last command sent was: %s" cmd e (Maxima.defaultInt.Value.ConsoleSession.LastLineSent)

    let kill (v:Var) =
        match Maxima.send' <| sprintf "kill(%s);" v.Name with
        | Ok r -> if r.Trim() <> "done" then failwithf "Could not kill symbol %A. Maxima returned %s." v r
        | Error e -> failwithf "Could not kill symbol %A. Maxima returned %s." v e.Message

    let declare_constant (v:Var) =
        match Maxima.send' <| sprintf "declare(%s, constant);" v.Name with
        | Ok r -> if r.Trim() <> "done" then failwithf "Could not declare symbol %A as constant. Maxima returned %s." v r
        | Error e -> failwithf "Could not declare symbol %A as constant. Maxima returned %s." v e.Message

    let assume (a:Expr<bool>) =
        let a' = sprint' a
        match Maxima.send' <| sprintf "assume((%s));" a'  with
        | Ok r -> if r.Trim() <> sprintf "[%s]" a' && r.Trim() <> "[redundant]" then failwithf "Could not add assumption %s. Unrecognized response: %s." a' r
        | Error e -> failwithf "Could not add assumption %s. Maxima returned: %s." a' (e.Message)

    let forget (a:Expr<bool>) =
        let a' = sprint' a
        match Maxima.send' <| sprintf "forget(%s);" a'  with
        | Ok _ -> ()
        | Error e -> failwithf "Could not forget assumption %s. Maxima returned: %s." a' (e.Message)

    let declare_constants v = List.iter declare_constant v

    let sum expr x l u = 
        assume <@ %u > %l @>
        let r = send expr <| sprintf "sum(%s, %s, %s, %s);" (sprint' expr) (sprint' x) (sprint' l) (sprint' u)
        forget <@ %u > %l @>
        r

    let limit expr x v =
        send expr <| sprintf "limit(%s, %s, %s);" (sprint' expr) (sprint' x) (sprint' v)
                
    let limit_right expr x v =
        send expr <| sprintf "limit(%s, %s, %s, plus);" (sprint' expr) (sprint' x) (sprint' v)

    let limit_left expr x v =
        send expr <| sprintf "limit(%s, %s, %s, minus);" (sprint' expr) (sprint' x) (sprint' v)

    let diff expr x n =
        send expr <| sprintf "diff(%s, %s, %i);" (sprint' expr) (sprint' x) n
        
    let integrate expr x = send expr <| sprintf "integrate(%s, %s);" (sprint' expr) (sprint' x)
    
    let definite_integral expr x l u = 
        let l' =
            match l with
            | NegInf -> "minf"
            | _ -> sprint' l
        let u' =
            match u with
            | PosInf -> "inf"
            | _ -> sprint' u

        do if l' <> "minf" && u' <> "inf" then assume <@ %u > %l @>
        let r = send expr <| sprintf "integrate(%s, %s, %s, %s);" (sprint' expr) (sprint' x) l' u'
        do if l' <> "minf" && u' <> "inf" then forget <@ %u > %l @>
        r