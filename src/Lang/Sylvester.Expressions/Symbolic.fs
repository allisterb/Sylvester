namespace Sylvester

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
        | Lambda(x, e) -> sprintf("%A = %s") x (sprint' e)
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
       
    let subst (e:Expr<'t>) (v:Expr<'u>) (r:Expr<'u>) =
        let var = get_var v
        e.Substitute(fun v -> if v.Name = var.Name && v.Type = var.Type then Some r.Raw else None) |> expand''<'t> |> simplify'

    let kronecker_delta<'t> (i:int) (j:int) = if i = j then one_val typeof<'t> else zero_val typeof<'t>