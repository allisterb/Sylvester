namespace Sylvester

open System
open FSharp.Quotations

// A binary relation between 2 sets
type IRelation<'a, 'b, 'c when 'a : equality and 'b : equality and 'c:equality> = 
    abstract member Domain: Set<'a>
    abstract member CoDomain: Set<'b>
    abstract member Op:Expr<'a->'b->bool>

type IRelation<'a, 'b when 'a : equality and 'b : equality> = IRelation<'a, 'b, 'a>

type IRelation<'t when 't: equality> = IRelation<'t, 't>

type Function<'a, 'b, 'c, 'd when 'a : equality and 'b: equality and 
    'c: equality and 'd:equality>(domain:ISet<'a>, codomain:ISet<'b>, map: Expr<'a->'b>, amap:Expr<'c->'a>, term:Expr<'b>->'d, ?symbol:string) =
    let amapbody = body' amap
    let amaparg = param_var amap
    member x.Domain = domain.Set
    member x.CoDomain = codomain.Set
    member x.MapExpr = map
    member x.Map = evaluate map
    member x.AMapExpr = amap
    member x.AMap = ev amap
    member x.Body = body' x.MapExpr
    member x.Vars = get_vars x.Body |> List.map Expr.Var
    member x.ArgVar = param_var x.MapExpr
    member x.ArgExpr = x.ArgVar |> Expr.Var |> expand_as<'a>
    member x.TermMap = term
    member val Symbol = symbol
    member val Attrs:System.Collections.Generic.Dictionary<string, obj> = new System.Collections.Generic.Dictionary<string, obj>()
    member x.SetAttr(n, o) = x.Attrs.[n] <- o; x
    abstract SubstArg:Expr->Expr<'b>
    default x.SubstArg(expr:Expr) = 
        let v = subst_var_value amaparg expr amapbody in subst_var_value x.ArgVar v x.Body |> expand_as<'b>        
    member x.Item (value:'c) = value |> exprv |> x.SubstArg |> term
        //arg |> x.AMap |> x.Map |> exprv |> x.TermMap
    member x.Item (value:Term<'c>) = value.Expr |> x.SubstArg |> term
    static member (|>>) (arg: 'c, f:Function<_,_,'c,_>) = f.[arg]
    interface IRelation<'a, 'b, 'c> with
        member x.Domain = x.Domain
        member x.CoDomain = x.CoDomain
        member x.Op = <@ fun a b -> x.Domain.HasElement a && x.CoDomain.HasElement b && b = x.Map a @> 

    (*
    interface ISymbolic<Function<'a, 'b, 'c, 'd>, 'b> with
           member a.Term = a
           member a.Expr = a.Body
           member a.Attrs = a.Attrs
           member a.Symbol = a.Symbol
           member a.Transform(b:Expr<'b>, ?attrs, ?s) = 
               let map = expand_as<'a->'b> (recombine_func (get_vars a.Body) b)
               let f = Function(a.Domain, a.CoDomain, map, a.AMapExpr, a.TermMap, ?symbol=s)
               do f.Attrs.Replace(defaultArg attrs null) |> ignore
               f
           member a.TransformWithSymbol(b:Expr<'b>, s:string) = 
                let map = expand_as<'a->'b> (recombine_func (get_vars a.Body) b)
                Function(a.Domain, a.CoDomain, map, a.AMapExpr, a.TermMap, s)
    *)
    interface IHtmlDisplay with member x.Html() = sprintf "$$ %s \mapsto %s $$" (latexe x.ArgExpr) (latexe x.Body)
    override x.ToString() = src x.MapExpr

type Function<'a, 'b, 'd when 'a : equality and 'b: equality and 'd: equality>(domain:ISet<'a>, codomain:ISet<'b>, map: Expr<'a->'b>, term:Expr<'b>->'d, ?symbol:string) = 
    inherit Function<'a, 'b, 'a, 'd>(domain, codomain, map, <@ id @>, term, ?symbol=symbol)

type ScalarFunction<'a, 'b, 'c when 'a : equality and 'b: equality and 'b: comparison and 'b :> ValueType and 'b :> IEquatable<'b> and 'c: equality>(domain:ISet<'a>, codomain:ISet<'b>, map: Expr<'a->'b>, amap:Expr<'c->'a>, ?symbol: string) = 
    inherit Function<'a, 'b, 'c, Scalar<'b>>(domain, codomain, map, amap, Scalar<'b>, ?symbol=symbol)
    
type ScalarFunction<'a, 'b when 'a : equality and 'b: equality and 'b: comparison and 'b :> ValueType and 'b :> IEquatable<'b>>(domain:ISet<'a>, codomain:ISet<'b>, map: Expr<'a->'b>, ?symbol:string) =
    inherit ScalarFunction<'a, 'b, 'a>(domain, codomain, map, <@ id @>, ?symbol=symbol)

type Predicate<'a when 'a : equality> = Function<'a, bool, Prop>

//type Predicate<'a when 'a : equality> = ScalarFunction<'a, bool>

[<AutoOpen>]
module Function =
    let farg (f:Function<'a,_,_,_>) = f.ArgExpr |> ScalarVar<'a>

    let fexpr (f:Function<_,'b,_,_>) = f.Body |> Scalar<'b>