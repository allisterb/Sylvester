namespace Sylvester

open System.Collections.Generic

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open MathNet.Symbolics

open MathNetExpr

[<AutoOpen>]
module Symbolic =
    /// Create a symbolic variable   
    let symbolic_var'<'t> n = let v = Expr.Var(Var(n, typeof<'t>)) in <@ %%v:'t @>

    (* Create sequences of variables *)
    
    let var'<'t> v = symbolic_var'<'t> v
    let var2'<'t> v1 v2 = symbolic_var'<'t> v1, symbolic_var'<'t> v2
    let var3'<'t> v1 v2 v3 = symbolic_var'<'t> v1, symbolic_var'<'t> v2, symbolic_var'<'t> v3
    let var4'<'t> v1 v2 v3 v4 = symbolic_var'<'t> v1, symbolic_var'<'t> v2, symbolic_var'<'t> v3, symbolic_var'<'t> v4
    
    let var_seq<'t> (s:string) n = seq {for i in 0..n - 1-> var'<'t> <| sprintf "%s%i" s i }

    let vars<'t> s n  = var_seq<'t> s n |> Seq.toArray
    
    
    (* Get quotation from type *)

    let inline sexpr (x : ^T) = (^T : (member Expr : Expr<'t>) (x))

    let inline sexprl (x : ^T) = (^T : (member Expr : Expr<'t> list) (x))

    let inline sexprs(a:'t[]) = a |> Array.map sexpr
    
    let inline sexprs'(a:'t [] []) = a |> Array.map(Array.map sexpr)

    
    (* Print quotation as string *)

    let rec sprint' (x:Expr) = 
        match x with
        | List l -> "[" + (l |>  List.map sprint' |> List.reduce (fun l r -> l + ", " + r)) + "]"
        | SpecificCall <@@ (<) @@> (_, _, [l; r]) -> sprintf("%s < %s") (l |> expand |> MathNetExpr.fromQuotation |> Infix.format) (r |> expand |> MathNetExpr.fromQuotation |> Infix.format)
        | SpecificCall <@@ (<=) @@> (_, _, [l; r]) -> sprintf("%s <= %s") (l |> expand |> MathNetExpr.fromQuotation |> Infix.format) (r |> expand |> MathNetExpr.fromQuotation |> Infix.format)
        | SpecificCall <@@ (>) @@> (_, _, [l; r]) -> sprintf("%s > %s") (l |> expand |> MathNetExpr.fromQuotation |> Infix.format) (r |> expand |> MathNetExpr.fromQuotation |> Infix.format)
        | SpecificCall <@@ (>=) @@> (_, _, [l; r]) -> sprintf("%s >= %s") (l |> expand |> MathNetExpr.fromQuotation |> Infix.format) (r |> expand |> MathNetExpr.fromQuotation |> Infix.format)
        | SpecificCall <@@ (=) @@> (_, _, [l; r]) -> sprintf("%s = %s") (l |> expand |> MathNetExpr.fromQuotation |> Infix.format) (r |> expand |> MathNetExpr.fromQuotation |> Infix.format)
        | _ -> x |> expand |> MathNetExpr.fromQuotation |> Infix.format

    let sprint_noexpand (x:Expr<'t>) = 
        match x with
        | SpecificCall <@@ (<) @@> (_, _, [l; r]) -> sprintf("%s < %s") (l |> MathNetExpr.fromQuotation |> Infix.format) (r |> MathNetExpr.fromQuotation |> Infix.format)
        | SpecificCall <@@ (<=) @@> (_, _, [l; r]) -> sprintf("%s <= %s") (l |> MathNetExpr.fromQuotation |> Infix.format) (r |> MathNetExpr.fromQuotation |> Infix.format)
        | SpecificCall <@@ (>) @@> (_, _, [l; r]) -> sprintf("%s > %s") (l |> MathNetExpr.fromQuotation |> Infix.format) (r |> MathNetExpr.fromQuotation |> Infix.format)
        | SpecificCall <@@ (>=) @@> (_, _, [l; r]) -> sprintf("%s >= %s") (l |> MathNetExpr.fromQuotation |> Infix.format) (r |> MathNetExpr.fromQuotation |> Infix.format)
        | SpecificCall <@@ (=) @@> (_, _, [l; r]) -> sprintf("%s = %s") (l |> MathNetExpr.fromQuotation |> Infix.format) (r |> MathNetExpr.fromQuotation |> Infix.format)
        | _ -> x |> MathNetExpr.fromQuotation |> Infix.format

    let latex' (x:Expr<'t>) = 
        match x with
        | SpecificCall <@@ (<) @@> (_, _, [l; r]) -> sprintf("%s < %s") (l |> MathNetExpr.fromQuotation |> LaTeX.format) (r |> MathNetExpr.fromQuotation |> LaTeX.format)
        | SpecificCall <@@ (<=) @@> (_, _, [l; r]) -> sprintf("%s <= %s") (l |> MathNetExpr.fromQuotation |> LaTeX.format) (r |> MathNetExpr.fromQuotation |> LaTeX.format)
        | SpecificCall <@@ (>) @@> (_, _, [l; r]) -> sprintf("%s > %s") (l |> MathNetExpr.fromQuotation |> LaTeX.format) (r |> MathNetExpr.fromQuotation |> LaTeX.format)
        | SpecificCall <@@ (>=) @@> (_, _, [l; r]) -> sprintf("%s >= %s") (l |> MathNetExpr.fromQuotation |> LaTeX.format) (r |> MathNetExpr.fromQuotation |> LaTeX.format)
        | SpecificCall <@@ (=) @@> (_, _, [l; r]) -> sprintf("%s = %s") (l |> MathNetExpr.fromQuotation |> LaTeX.format) (r |> MathNetExpr.fromQuotation |> LaTeX.format)
        | _ -> x |> MathNetExpr.fromQuotation |> LaTeX.format

    let inline sprint expr = expr |> sexpr |> expand |> MathNetExpr.fromQuotation |> Infix.format

    let simplify' (x:Expr<'t>) = x |> callUnary<'t> id

    let inline simplify expr = expr |> sexpr |> simplify'
       
    let kronecker_delta<'t> (i:int) (j:int) = if i = j then one_val typeof<'t> else zero_val typeof<'t>
    
    let algebraic_expand x = x |> callUnary Algebraic.expand 
       
    let subst (e:Expr<'t>) (v:Expr<'u>) (r:Expr<'u>) =
        let var = get_var v
        e.Substitute(fun v -> if v.Name = var.Name && v.Type = var.Type then Some r.Raw else None) |> expand''<'t> |> simplify'

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