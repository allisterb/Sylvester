namespace Sylvester

open System 

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open MathNet.Symbolics

open MathNetExpr

type IExpr<'t> =
    abstract member Expr:Expr<'t>

type IExprs<'t> =
    abstract member Exprs: Expr<'t> []

type IExprs2<'t> =
    abstract member Exprs: Expr<'t> [][]

type IAttrs =
    abstract member Attrs: System.Collections.Generic.Dictionary<string, obj>

type ISymbolic<'s, 't> = 
    inherit IExpr<'t>
    inherit IAttrs
    abstract member Term: 's
    abstract member Symbol: string option
    abstract member Transform : e:Expr<'t> * ?a:System.Collections.Generic.Dictionary<string, obj> * ?s:string-> 's
    
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
    
    (* Patterns *)
    let (|NumericExpr|_|):obj->option<unit> =
        function
        | :? IExprs<_> as e when exprs_all_numeric e.Exprs -> Some ()
        | _ -> None

    (* Get quotation from type *)

    let inline sexpr (x : ^T) = (^T : (member Expr : Expr<'t>) (x))

    let inline sexpr_l (x : ^T) = (^T : (member Expr : Expr<'t> list) (x))

    let inline sexprs(a:'t[]) = a |> Array.map sexpr
    
    let inline sexprs2(a:'t [] []) = a |> Array.map(Array.map sexpr)

    let simplifye (x:Expr<'t>) = x |> callUnary<'t> id
    
    let newattrs ((p:(string*obj) list)) =
        let a = new System.Collections.Generic.Dictionary<string, obj>()
        p |> List.iter(fun (k,v) -> a.[k] <- v)
        a
    let with_attr n o (s:ISymbolic<_,_>) = s.Attrs.[n] <- o; s.Term

    let with_symbol n (s:ISymbolic<_,_>) = s.Transform(s.Expr, null, n)

    let inline with_attr_tag n (x : ^T)  = (^T : (member Attrs : System.Collections.Generic.Dictionary<string, obj>) (x)).[n] <- true; x

    let inline has_attr_tag n (x : ^T)  = (^T : (member Attrs : System.Collections.Generic.Dictionary<string, obj>) (x)).ContainsKey(n)

    let fix (attrs:'a) (s:ISymbolic<_,'b>) =
        let mutable m = s.Expr.Raw
        get_consts m |> List.iter(fun (t, n) -> 
            if has_prop n typeof<'b> attrs then 
                let s = get_prop n typeof<'b> attrs :?> 'b in 
                let v = exprv s in 
                m <- replace_expr (Expr.ValueWithName(Unchecked.defaultof<'b>, n)) v m
                m <- replace_expr (expr_var<'b> n) v m
        )
        s.Transform(expand_as<'b> m |> simplifye, null, ?s=s.Symbol)
        
    (* Term patterns *)
    let (|Atom|_|) expr =
        match expr with
        | Var _ 
        | Value(_, _) 
        | ValueWithName(_,_,_) -> Some expr
        | _ -> None

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

    let rec latexe (x:Expr) = 
        match x with
        | List list -> "[" + (list |>  List.map latexe |> List.reduce (fun l r -> l + ", " + r)) + "]"
        | SpecificCall <@@ (<) @@> (_, _, [l; r]) -> sprintf("%s < %s") (latexe l) (latexe r)
        | SpecificCall <@@ (<=) @@> (_, _, [l; r]) -> sprintf("%s <= %s") (latexe l) (latexe r)
        | SpecificCall <@@ (>) @@> (_, _, [l; r]) -> sprintf("%s > %s") (latexe l) (latexe r)
        | SpecificCall <@@ (>=) @@> (_, _, [l; r]) -> sprintf("%s >= %s") (latexe l) (latexe r)
        | SpecificCall <@@ (=) @@> (_, _, [l; r]) -> sprintf("%s = %s") (latexe l) (latexe r)

        | SpecificCall <@@ (+) @@> (_, _, [Double -1.0; r]) -> sprintf("%s - %s") (latexe r) (latexe <@ 1.0 @>)
        | SpecificCall <@@ (+) @@> (_, _, [l; r]) -> sprintf("%s + %s") (latexe l) (latexe r)
        | SpecificCall <@@ (+) @@> (_, _, [l; (SpecificCall <@@ (*) @@> (_, _, [Double 1.0; r]))]) -> sprintf("%s - %s") (latexe l) (latexe r)
        | SpecificCall <@@ (-) @@> (_, _, [l; r]) -> sprintf("%s - %s") (latexe l) (latexe r)
        | SpecificCall <@@ (*) @@> (_, _, [ValueWithName(_,_, _) as l; r]) -> sprintf("{%s}{%s}") (latexe l) (latexe <| r)
        | SpecificCall <@@ (*) @@> (_, _, [l; ValueWithName(_,_, _) as r]) -> sprintf("{%s}{%s}") (latexe l) (latexe r)
        | SpecificCall <@@ (*) @@> (_, _, [Double l; Double r]) -> sprintf("%s\cdot%s") (latexe <| exprv l) (latexe <|  exprv r)
        | SpecificCall <@@ (*) @@> (_, _, [l; Call(None, Op "Identity", Double r::[])]) -> sprintf("%s\cdot%s") (latexe l) (latexe <|  exprv r)
        | SpecificCall <@@ (*) @@> (_, _, [Call(None, Op "Identity", Double l::[]); r]) -> sprintf("%s\cdot%s") (latexe <| exprv l) (latexe r)
        | SpecificCall <@@ (*) @@> (_, _, [l; r]) -> sprintf("%s%s") (latexe l) (latexe r)
        | SpecificCall <@@ (/) @@> (_, _, [l; r]) -> sprintf("\\frac{%s}{%s}") (latexe l) (latexe r)
        | SpecificCall <@@ ( ** ) @@> (_, _, [l; r]) -> sprintf("%s^{%s}") (latexe l) (latexe r)

        | Call(None, Op "Exp", x::[]) -> sprintf("exp(%s)") (latexe x)

        | Lambda(v, x) -> sprintf "%s \mapto %s" (latexe (Expr.Var v)) (latexe x)
        | Var x when x.Name.EndsWith "_bar" -> 
            if Symbols.TransliterateGreek && Symbols.isGreek (x.Name.Delete("_bar")) then 
                sprintf "\\bar{%s}" Symbols.GreekLatex.[x.Name.Delete("_bar")] 
            else
                sprintf "\\bar{%s}" (x.Name.Delete("_bar"))
        | Var x -> if Symbols.TransliterateGreek && Symbols.isGreek (x.Name) then Symbols.GreekLatex.[x.Name] else x.Name
        
        | ValueWithName(_,_,n) when n.EndsWith "_bar" -> 
            if Symbols.TransliterateGreek && Symbols.isGreek (n.Delete("_bar")) then 
                sprintf "\\bar{%s}" Symbols.GreekLatex.[n.Delete("_bar")] 
            else
                sprintf "\\bar{%s}" (n.Delete("_bar"))
        | ValueWithName(_, _, n) -> if Symbols.TransliterateGreek && Symbols.isGreek n then Symbols.GreekLatex.[n] else n

        | Double d when d = Math.Floor(d + 0.00001) ->  latexe <| Expr.Value (Convert.ToInt32(d))

        | _ -> x |> MathNetExpr.fromQuotation |> LaTeX.format

    let inline sprints expr = expr |> sexpr |> expand |> MathNetExpr.fromQuotation |> Infix.format

    
    let inline simplify expr = expr |> sexpr |> simplifye
       
    let subst (e:Expr<'t>) (v:Expr<'u>) (r:Expr<'u>) =
        let var = get_var v
        e.Substitute(fun v -> if v.Name = var.Name && v.Type = var.Type then Some r.Raw else None) |> expand_as<'t> |> simplifye

    let kronecker_delta<'t> (i:int) (j:int) = if i = j then one_val typeof<'t> else zero_val typeof<'t>