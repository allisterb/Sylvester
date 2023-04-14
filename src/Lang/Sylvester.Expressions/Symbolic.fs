namespace Sylvester

open System 

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open MathNet.Symbolics

open MathNetExpr

type ISymbolic<'s, 't> = 
    abstract member Expr:Expr<'t>
    abstract member Mutate : Expr<'t> -> 's
    
[<AutoOpen>]
module Symbolic =
    /// Create a symbolic variable   
    let symbolic_var<'t> n = let v = Expr.Var(Var(n, typeof<'t>)) in <@ %%v:'t @>

    (* Create sequences of variables *)
    let var'<'t> v = symbolic_var<'t> v
    let var2'<'t> v1 v2 = symbolic_var<'t> v1, symbolic_var<'t> v2
    let var3'<'t> v1 v2 v3 = symbolic_var<'t> v1, symbolic_var<'t> v2, symbolic_var<'t> v3
    let var4'<'t> v1 v2 v3 v4 = symbolic_var<'t> v1, symbolic_var<'t> v2, symbolic_var<'t> v3, symbolic_var<'t> v4
    
    
    let var_seq<'t> (s:string) n = seq {for i in 0..n - 1-> symbolic_var<'t> <| sprintf "%s%i" s i }

    let vars<'t> s n  = var_seq<'t> s n |> Seq.toArray
    
    
    (* Get quotation from type *)

    let inline sexpr (x : ^T) = (^T : (member Expr : Expr<'t>) (x))

    let inline sexpr_l (x : ^T) = (^T : (member Expr : Expr<'t> list) (x))

    let inline sexprs(a:'t[]) = a |> Array.map sexpr
    
    let inline sexprs2(a:'t [] []) = a |> Array.map(Array.map sexpr)

    
    (* Print quotation as string *)

    let rec sprinte (x:Expr) = 
        match x with
        | List list -> "[" + (list |>  List.map sprinte |> List.reduce (fun l r -> l + ", " + r)) + "]"
        | SpecificCall <@@ (<) @@> (_, _, [l; r]) -> sprintf("%s < %s") (sprinte l) (sprinte r)
        | SpecificCall <@@ (<=) @@> (_, _, [l; r]) -> sprintf("%s <= %s") (sprinte l) (sprinte r)
        | SpecificCall <@@ (>) @@> (_, _, [l; r]) -> sprintf("%s > %s") (sprinte l) (sprinte r)
        | SpecificCall <@@ (>=) @@> (_, _, [l; r]) -> sprintf("%s >= %s") (sprinte l) (sprinte r)
        | SpecificCall <@@ (=) @@> (_, _, [l; r]) -> sprintf("%s = %s") (sprinte l) (sprinte r)
        
        | SpecificCall <@@ (+) @@> (_, _, [l; r]) -> sprintf("%s + %s") (sprinte l) (sprinte r)
        | SpecificCall <@@ (-) @@> (_, _, [l; r]) -> sprintf("%s - %s") (sprinte l) (sprinte r)
        | SpecificCall <@@ (*) @@> (_, _, [l; r]) -> sprintf("%s * %s") (sprinte l) (sprinte r)
        | SpecificCall <@@ (/) @@> (_, _, [l; r]) -> sprintf("%s / %s") (sprinte l) (sprinte r)
        | SpecificCall <@@ ( ** ) @@> (_, _, [l; r]) -> sprintf("%s^%s") (sprinte l) (sprinte r)
        
        | Call(None, Op "Exp", x::[]) -> sprintf("e^%s") (sprinte x)
        | Call(None, Op "Identity", x::[]) -> (sprinte x)
        | PropertyGet(None, Prop "e", []) -> "e"
        | PropertyGet(None, Prop "pi", []) -> "pi"
        | Bool false -> "false"
        | Bool true -> "true"
        
        | Var x as v -> if Symbols.TransliterateGreek && Symbols.isGreek (x.Name) then Symbols.GreekUnicode.[x.Name] else x.Name  
        | Lambda(x, e) -> sprintf("%A = %s") x (sprinte e)
        
        
        | _ -> x |> expand |> MathNetExpr.fromQuotation |> Infix.format

    let sprintel (exprs: Expr<'t> list) =
        exprs 
        |> List.toArray
        |> Array.skip 1 
        |> Array.fold(fun s e -> sprintf "%s, %s" s (sprinte e)) (sprinte exprs.[0]) 
        |> sprintf "[%s]"

    let rec latex' (x:Expr) = 
        match x with
        | List list -> "[" + (list |>  List.map latex' |> List.reduce (fun l r -> l + ", " + r)) + "]"
        | SpecificCall <@@ (<) @@> (_, _, [l; r]) -> sprintf("%s < %s") (latex' l) (latex' r)
        | SpecificCall <@@ (<=) @@> (_, _, [l; r]) -> sprintf("%s <= %s") (latex' l) (latex' r)
        | SpecificCall <@@ (>) @@> (_, _, [l; r]) -> sprintf("%s > %s") (latex' l) (latex' r)
        | SpecificCall <@@ (>=) @@> (_, _, [l; r]) -> sprintf("%s >= %s") (latex' l) (latex' r)
        | SpecificCall <@@ (=) @@> (_, _, [l; r]) -> sprintf("%s = %s") (latex' l) (latex' r)

        | SpecificCall <@@ (+) @@> (_, _, [l; r]) -> sprintf("%s + %s") (latex' l) (latex' r)
        | SpecificCall <@@ (-) @@> (_, _, [l; r]) -> sprintf("%s - %s") (latex' l) (latex' r)
        | SpecificCall <@@ (*) @@> (_, _, [l; r]) -> sprintf("%s * %s") (latex' l) (latex' r)
        | SpecificCall <@@ (/) @@> (_, _, [l; r]) -> sprintf("%s / %s") (latex' l) (latex' r)
        | SpecificCall <@@ ( ** ) @@> (_, _, [l; r]) -> sprintf("%s^%s") (latex' l) (latex' r)

        | Call(None, Op "Exp", x::[]) -> sprintf("exp(%s)") (latex' x)

        | Var x -> if Symbols.TransliterateGreek && Symbols.isGreek (x.Name) then Symbols.GreekLatex.[x.Name] else x.Name
        
        | Double d when d = Math.Floor(d + 0.00001) ->  latex' <| Expr.Value (Convert.ToInt32(d))

        | _ -> x |> MathNetExpr.fromQuotation |> LaTeX.format

    let inline sprints expr = expr |> sexpr |> expand |> MathNetExpr.fromQuotation |> Infix.format

    //let sprint (s:ISymbolic<_, _>) = s.
    let simplifye (x:Expr<'t>) = x |> callUnary<'t> id

    let inline simplify expr = expr |> sexpr |> simplifye
       
    let subst (e:Expr<'t>) (v:Expr<'u>) (r:Expr<'u>) =
        let var = get_var v
        e.Substitute(fun v -> if v.Name = var.Name && v.Type = var.Type then Some r.Raw else None) |> expand_as<'t> |> simplifye

    let kronecker_delta<'t> (i:int) (j:int) = if i = j then one_val typeof<'t> else zero_val typeof<'t>