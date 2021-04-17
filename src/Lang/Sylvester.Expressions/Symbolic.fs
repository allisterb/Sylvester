namespace Sylvester

open System.Collections.Generic

open FSharp.Quotations
open FSharp.Quotations.Patterns

open MathNet.Symbolics

open MathNetExpr

[<RequireQualifiedAccess>]
module SymbolicOps =    
    let AlgebraicBinaryOps = new Dictionary<string, Dictionary<string, Expr -> Expr -> Expr>>()
    
    let addBinary<'t> name op = AlgebraicBinaryOps.Add(name, op)

    let findAlgebraicBinary<'t> op (l:Expr<'t>) (r:Expr<'t>) = 
        if AlgebraicBinaryOps.ContainsKey typeof<'t>.Name && AlgebraicBinaryOps.[typeof<'t>.Name].ContainsKey op then 
            AlgebraicBinaryOps.[typeof<'t>.Name].[op] l.Raw r.Raw  
        else failwithf "No symbolic %s operation implemented for type %A." op typeof<'t>

    let Add<'t> = findAlgebraicBinary<'t> "Add"
    let Mul<'t> = findAlgebraicBinary<'t> "Mul"
    let Sub<'t> = findAlgebraicBinary<'t> "Sub"
    let Divide<'t> = findAlgebraicBinary<'t> "Divide"
    let Pow<'t> = findAlgebraicBinary<'t> "Pow"

[<AutoOpen>]
module Symbolic =
    let alg_simplify x = x |> expand |> callUnary id 
        
    let alg_expand x = x |> expand |> callUnary Algebraic.expand 
        
    //let term_simplify (t:Term<_>) = Term()
    let polyn_coeffs e x = 
        let x' = x |> expand in
        x'
        |> fromQuotation 
        |> Polynomial.coefficients (e |> expand |> fromQuotation) 
        |> Array.map (toQuotation (x' |> get_vars))
        |> Array.map(Option.get)
        |> Array.toList

    let polyn_coeff term x = 
        let x' = x |> expand in
        match x' |> polyn_coeffs term with
        | _::c::[] -> c
        | _ -> failwithf "Expression %s is not an algebraic expression." <| src term
    
    let polyn_coeff_val term (x:Expr<'t>) =
        let x' = x |> expand in
        match x' |> polyn_coeff term with
        | Value(v, t) when t = x.Type -> v :?> 't
        | _ -> failwithf "The expression %s does not have a term %s with coefficient type %s." (src x) (src term) (term.Type.ToString())

    let polyn_all_coeffs x = 
        let x' = x |> expand in
        x'  
        |> get_vars 
        |> List.sortBy(fun v -> v.Name) 
        |> List.map (fun v -> v, polyn_coeff (Expr.Var v) x')

    let polyn_all_coeffs_val x = 
        let x' = x |> expand  in
        x' 
        |> get_vars 
        |> List.sortBy(fun v -> v.Name) 
        |> List.map (fun v -> v, polyn_coeff_val (Expr.Var v) x)
    
    let polyn_degree (x:Expr) = 
        let x' = expand x in 
        get_vars x'
        |> List.map (fun v -> x' |> fromQuotation |> (Polynomial.degree (v |> toIdentifier)))
        |> List.map (toQuotation [])
        |> List.map (Option.get)
        |> List.map (fun e -> match e with | Value(v, t) -> v :?> float | _ -> failwith "Unexpected expression in degree.")
        |> List.max
        |> int
    
    let polyn_eqn_all_coeffs (x:Expr<bool list>) =
        let am = expand_list x |> List.map expand_equality
        let cm = am |> List.map (fst >> polyn_all_coeffs)
        let vm = am |> List.map snd
        cm, vm

    let polyn_eqn_is_linear x = 
        let l, r = expand_equality x in 
        polyn_degree l = 1 && polyn_degree r = 0   
