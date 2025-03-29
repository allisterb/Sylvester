namespace Sylvester

open System 
open System.IO

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns


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
    
type ISymbolicExpr<'s, 't> =
    abstract member SymbolicExpr:Expr<'t>

type IDescription =
    abstract member Html:string
    abstract member Text:string

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

    let simplifye (x:Expr<'t>) = x |> callUnary<'t> id
    
    let newattrs ((p:(string*obj) list)) =
        let a = new System.Collections.Generic.Dictionary<string, obj>()
        p |> List.iter(fun (k,v) -> a.[k] <- v)
        a
    let with_attr n o (x:'a when 'a :> IAttrs) = (x :> IAttrs).Attrs.[n] <- o; x

    let with_symbol n (s:ISymbolic<_,_>) = s.Transform(s.Expr, null, n)

    let with_attr_tag n (x : 'a when 'a :> IAttrs) = (x :> IAttrs).Attrs.[n] <- true; x

    let has_attr_tag n (x : 'a when 'a :> IAttrs)  = (x :> IAttrs).Attrs.ContainsKey(n)

    let fix (attrs:'a) (s:'s when 's :> #ISymbolic<'s,'b>) :'s =
        let mutable m = s.Expr.Raw
        get_consts m |> List.iter(fun (t, n) -> 
            let name = n.Replace("_", "").Replace("^", "")
            if has_prop<'b> name attrs then 
                let s = get_prop<'b> name attrs in 
                let v = exprv s in 
                m <- replace_expr (Expr.ValueWithName(Unchecked.defaultof<'b>, n)) v m
                m <- replace_expr (expr_var<'b> n) v m
        )
        get_vars m |> List.iter(fun v -> 
            if has_prop<'b> v.Name attrs then  
                let s = get_prop<'b> v.Name attrs in 
                let co = exprv s in  
                m <- replace_expr (Expr.Var v) co m
        )
        s.Transform(expand_as<'b> m |> simplifye, null, ?s=s.Symbol)
        
    let fixconst (c:seq<string>) (s:'s when 's :> #ISymbolic<'s,'b>) :'s =
        let mutable m = s.Expr.Raw
        get_vars m |> List.iter(fun v -> 
            if Seq.contains v.Name c then  
                let co = Expr.ValueWithName(Unchecked.defaultof<'b>, v.Name) in 
                m <- replace_expr (Expr.Var v) co m
        )
        s.Transform(expand_as<'b> m |> simplifye, null, ?s=s.Symbol)

    let const_to_var (c:seq<string>) (s:'s when 's :> #ISymbolic<'s,'b>) :'s =
           let mutable m = s.Expr.Raw
           get_consts m |> List.iter(fun (t,n) -> 
               if Seq.contains n c then  
                   let co = Expr.Var(Var(n, t)) in 
                   m <- replace_expr (Expr.ValueWithName(Unchecked.defaultof<'b>, n)) co m
           )
           s.Transform(expand_as<'b> m |> simplifye, null, ?s=s.Symbol)

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
        | SpecificCall <@@ (*) @@> (_, _, [Double 1.; r]) -> sprintf("%s") (sprinte r)
        | SpecificCall <@@ (*) @@> (_, _, [Double -1.; Atom r]) -> sprintf("-%s") (sprinte r)
        | SpecificCall <@@ (*) @@> (_, _, [Double -1.; r]) -> sprintf("-(%s)") (sprinte r)
        | SpecificCall <@@ (*) @@> (_, _, [Atom l; Atom r]) -> sprintf("%s * %s") (sprinte l) (sprinte r)
        | SpecificCall <@@ (*) @@> (_, _, [Atom l; r]) -> sprintf("%s * (%s)") (sprinte l) (sprinte r)
        | SpecificCall <@@ (*) @@> (_, _, [l; Atom r]) -> sprintf("(%s) * %s") (sprinte l) (sprinte r)
        | SpecificCall <@@ (*) @@> (_, _, [Atom l; Atom r]) -> sprintf("(%s) * (%s)") (sprinte l) (sprinte r)
        | SpecificCall <@@ (*) @@> (_, _, [l; r]) -> sprintf("%s * %s") (sprinte l) (sprinte r)
        | SpecificCall <@@ (/) @@> (_, _, [l; r]) -> sprintf("%s / %s") (sprinte l) (sprinte r)
        | SpecificCall <@@ ( ** ) @@> (_, _, [Atom l; r]) -> sprintf("%s^%s") (sprinte l) (sprinte r)
        | SpecificCall <@@ ( ** ) @@> (_, _, [l; r]) -> sprintf("(%s)^%s") (sprinte l) (sprinte r)

        | SpecificCall <@@ (<|) @@> (_, _, [l; r]) when src l = "real" -> (sprinte r)
        | SpecificCall <@@ (|>) @@> (_, _, [l;  Lambda (n1, Call (None, real, [n2]))]) -> (sprinte l)

        | Call(None, Op "Exp", Atom x::[]) -> sprintf("e^%s") (sprinte x)
        | Call(None, Op "Identity", x::[]) -> (sprinte x)
        | Call(None, Op "real", x::[]) -> (sprinte x)
        | Call(None, Op "min", l::r::[]) -> sprintf("min(%s,%s)") (sprinte l) (sprinte r)
        | Call(None, Op "log", x::[]) -> sprintf "ln(%s)"(sprinte x)

 
        | PropertyGet(None, Prop "e", []) -> "e"
        | PropertyGet(None, Prop "pi", []) -> "pi"
        | Bool false -> "false"
        | Bool true -> "true"
        
        | ValueWithName(_,_,n) -> n
        | Double (Double.MaxValue) -> "inf"
        | Double (Double.MinValue) -> "neginf"
        | Call(None, Op "real_frac", Int32 n::Int32 d::[]) -> sprintf "%A/%A" n d
        | SpecificCall <@@ symbolic_fn @@> (_,_,[String s;NewArray(_, v)]) -> sprintf "%s(%s)" s (v |> List.map (function | String s ->  s | _ -> failwith "") |> List.reduce (sprintf "%s,%s"))

        | Var x as v -> if Symbols.TransliterateGreek && Symbols.isGreek (x.Name) then Symbols.GreekUnicode.[x.Name] else x.Name  
        | Lambda(x, e) -> sprintf("%A = %s") x (sprinte e)        
        | Double d when d = Math.Floor(d + 0.00001) ->  sprinte <| Expr.Value (Convert.ToInt32(d))

        | _ -> x |> expand |> MathNetExpr.fromQuotation |> MathNet.Symbolics.Infix.format

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

        | SpecificCall <@@ (*) @@> (_, _, [Double 1.0; r]) -> latexe r 
        | SpecificCall <@@ (+) @@> (_, _, [l;SpecificCall <@@ (*) @@> (_, _, [Double -1.0; Atom r])]) -> sprintf("%s - %s") (latexe l) (latexe r)
        | SpecificCall <@@ (+) @@> (_, _, [l;SpecificCall <@@ (*) @@> (_, _, [Double -1.0; r])]) -> sprintf("%s - (%s)") (latexe l) (latexe r)
        | SpecificCall <@@ (+) @@> (_, _, [Double l as d; r]) when l < 0. -> sprintf("%s - %s") (latexe r) (latexe (Expr.Value(-l)))
        | SpecificCall <@@ (+) @@> (_, _, [l; r]) -> sprintf("%s + %s") (latexe l) (latexe r)
        | SpecificCall <@@ (-) @@> (_, _, [l; r]) -> sprintf("%s - %s") (latexe l) (latexe r)
        | SpecificCall <@@ (*) @@> (_, _, [ValueWithName(_,_, _) as l; r]) -> sprintf("{%s}{%s}") (latexe l) (latexe <| r)
        | SpecificCall <@@ (*) @@> (_, _, [l; ValueWithName(_,_, _) as r]) -> sprintf("{%s}{%s}") (latexe l) (latexe r)
        | SpecificCall <@@ (*) @@> (_, _, [Double l; Double r]) -> sprintf("%s\cdot%s") (latexe <| exprv l) (latexe <|  exprv r)
        | SpecificCall <@@ (*) @@> (_, _, [l; Call(None, Op "Identity", Double r::[])]) -> sprintf("%s\cdot%s") (latexe l) (latexe <|  exprv r)
        | SpecificCall <@@ (*) @@> (_, _, [Call(None, Op "Identity", Double l::[]); r]) -> sprintf("%s\cdot%s") (latexe <| exprv l) (latexe r)
        | SpecificCall <@@ (*) @@> (_, _, [Atom l; Atom r]) -> sprintf("%s%s") (latexe l) (latexe r)
        | SpecificCall <@@ (*) @@> (_, _, [l; Atom r]) -> sprintf("%s%s") (latexe l) (latexe r)
        | SpecificCall <@@ (*) @@> (_, _, [l; SpecificCall <@@ ( ** ) @@> (_, _, [Atom b; Atom e])]) -> sprintf("%s%s^%s") (latexe l) (latexe b) (latexe e)
        | SpecificCall <@@ (*) @@> (_, _, [l; r]) -> sprintf("(%s)(%s)") (latexe l) (latexe r)
        | SpecificCall <@@ (/) @@> (_, _, [l; Atom r]) -> sprintf("\\frac{%s}{%s}") (latexe l) (latexe r)
        | SpecificCall <@@ (/) @@> (_, _, [l; r]) -> sprintf("\\frac{%s}{(%s)}") (latexe l) (latexe r)
        | SpecificCall <@@ ( ** ) @@> (_, _, [Atom l; Atom r]) -> sprintf("%s^{%s}") (latexe l) (latexe r)
        | SpecificCall <@@ ( ** ) @@> (_, _, [Atom l; r]) -> sprintf("%s^{(%s)}") (latexe l) (latexe r)
        | SpecificCall <@@ ( ** ) @@> (_, _, [l; Atom r]) -> sprintf("(%s)^{%s}") (latexe l) (latexe r)
        | SpecificCall <@@ ( ** ) @@> (_, _, [l; r]) -> sprintf("(%s)^{(%s)}") (latexe l) (latexe r)

        | Call(None, Op "Exp", x::[]) -> sprintf("exp(%s)") (latexe x)

        | Lambda(v, x) -> sprintf "%s \mapto %s" (latexe (Expr.Var v)) (latexe x)
        | Var x when x.Name.EndsWith "_bar" -> 
            let n = x.Name.Delete("_bar") in
            if Symbols.TransliterateGreek && Symbols.isGreek n then 
                sprintf "\\bar{%s}" Symbols.GreekLatex.[n] 
            else
                sprintf "\\bar{%s}" n
        | Var x -> if Symbols.TransliterateGreek && Symbols.isGreek (x.Name) then Symbols.GreekLatex.[x.Name] else x.Name
        
        | ValueWithName(_,_,n) when n.EndsWith "_bar" -> 
            if Symbols.TransliterateGreek && Symbols.isGreek (n.Delete("_bar")) then 
                sprintf "\\bar{%s}" Symbols.GreekLatex.[n.Delete("_bar")] 
            else
                sprintf "\\bar{%s}" (n.Delete("_bar"))
        | ValueWithName(_,_,n) -> if Symbols.TransliterateGreek && Symbols.isGreek n then Symbols.GreekLatex.[n] else n
        | Double d when d = Math.Floor(d + 0.00001) ->  latexe <| Expr.Value (Convert.ToInt32(d))
        | Double d ->
            let r = Rational d
            if r.Numerator.IsOne || (-r.Numerator).IsOne then sprintf "\\frac{%A}{%A}" r.Numerator r.Denominator else d.ToString()

        | _ -> x |> MathNetExpr.fromQuotation |> MathNet.Symbolics.LaTeX.format

    let inline latex x = x |> sexpr |> latexe 
    
    let inline sprints expr = expr |> sexpr |> sprinte

    let inline simplify expr = expr |> sexpr |> simplifye
       
    let subst (e:Expr<'t>) (v:Expr<'u>) (r:Expr<'u>) =
        let var = get_var v
        e.Substitute(fun v -> if v.Name = var.Name && v.Type = var.Type then Some r.Raw else None) |> expand_as<'t>

    let kronecker_delta<'t> (i:int) (j:int) = if i = j then one_val typeof<'t> else zero_val typeof<'t>

    type IHtmlDisplayFormatterSource() =
        member x.CreateTypeFormatters() = seq { yield new IHtmlDisplayFormatter() }

    and IHtmlDisplayFormatter() = 
        member val MimeType:string = "text/html"

        member x.Format(instance:obj, writer:TextWriter) = 
            match instance with
            | :? IHtmlDisplay as hd -> writer.Write(hd.Html()); true
            | _ -> false