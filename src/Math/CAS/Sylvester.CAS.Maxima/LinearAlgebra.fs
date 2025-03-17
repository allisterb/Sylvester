namespace Sylvester.CAS

open FSharp.Quotations

open Sylvester
open Maxima

open MathNet.Symbolics

module LinearAlgebra =
    
    let private send s = Maxima.send' s
    
    let private get_vars_m(expr:Expr<_>[][]) = expr |> Array.map (Array.map(get_vars >> List.toArray)) |> Array.concat |> Array.concat |> Array.toList

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

    let private sendMatrixCmd<'t> vars cmd = 
        cmd
        |> send
        |> Result.mapError(fun e -> sprintf "Error executing Maxima command %s: %s. Maxima session output: %s" cmd e.Message (Maxima.last_output 10))
        |> Result.bind(fun o -> MathNet.Symbolics.Infix.parseMatrix (o.Replace("%", "")))
        |> Result.map(List.map(List.map (MathNetExpr.toQuotation<'t> vars)))
        |> Result.mapError(fun e -> sprintf "Error parsing output of Maxima command %s: %s. Maxima session output: %s" cmd e (Maxima.last_output 10))
        |> function
        | Ok s -> s |> List.map (List.toArray) |> List.toArray
        | Error e -> failwith e
    
    let private sendListsCmd<'t> vars cmd = 
        cmd
        |> send
        |> Result.mapError(fun e -> sprintf "Error executing Maxima command %s: %s. Maxima session output: %s" cmd e.Message (Maxima.last_output 10))
        |> Result.bind(fun o -> MathNet.Symbolics.Infix.parseLists (o.Replace("%", "")))
        |> Result.map(List.map(List.map (MathNetExpr.toQuotation<'t> vars)))
        |> Result.mapError(fun e -> sprintf "Error parsing output of Maxima command %s: %s. Maxima session output: %s" cmd e (Maxima.last_output 10))
        |> function
        | Ok s -> s 
        | Error e -> failwith e

    let charpoly (v:Expr<'t>) (expr:Expr<'t>[][])  =
        let vars = get_vars_m expr @ [get_var v]
        sprintf "ratsimp(charpoly(%s,%s))" (sprintm expr) (sprinte v) |> sendCmd<'t> vars

    let echelon(expr:Expr<'t>[][]) = sprintf "echelon(%s);" (sprintm expr) |> sendMatrixCmd<'t> (get_vars_m expr)

    let jordan_normal_form (expr:Expr<'t>[][]) = sprintf "jordan(%s);" (sprintm expr) |> sendListsCmd<'t> (get_vars_m expr)

    let jordan_similar (blocklist:(Expr<'t>*Expr<int>) []) (mexpr:Expr<'t>[][]) = 
        let l = blocklist |> Array.map(fun (e,n) -> sprintf "[%s,%s]" (sprinte e) (sprinte n)) |> Array.reduce (sprintf "%s,%s") |> sprintf "[%s]"
        let lv = blocklist |> Array.collect (fst >> get_vars >> List.toArray) |> Array.toList
        sprintf "ModeMatrix(%s, %s);" (sprintm mexpr) l |> sendMatrixCmd<'t> (get_vars_m mexpr @ lv)
