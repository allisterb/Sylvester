namespace Sylvester

open FSharp.Quotations

// A binary relation between 2 sets
type IRelation<'a, 'b, 'c when 'a : equality and 'b : equality and 'c:equality> = 
    abstract member Domain: Set<'a>
    abstract member CoDomain: Set<'b>
    abstract member Op:Expr<'a->'b->bool>

type IRelation<'a, 'b when 'a : equality and 'b : equality> = IRelation<'a, 'b, 'a>

type IRelation<'t when 't: equality> = IRelation<'t, 't>

type Function<'a, 'b, 'c when 'a : equality and 'b: equality and 'c: equality>(domain:ISet<'a>, codomain:ISet<'b>, map: Expr<'a->'b>, amap:Expr<'c->'a>) =
    let amapbody = body' amap
    let amaparg = param_var amap
    member x.Domain = domain.Set
    member x.CoDomain = codomain.Set
    member x.Map = map
    member x.EvMap = evaluate map
    member x.AMap = amap
    member x.EvAMap = ev amap
    member x.Body = body' x.Map
    member x.Vars = get_vars x.Body
    member x.Arg = param_var x.Map

    interface IRelation<'a, 'b, 'c> with
        member x.Domain = x.Domain
        member x.CoDomain = x.CoDomain
        member x.Op = <@ fun a b -> x.Domain.HasElement a && x.CoDomain.HasElement b && b = x.EvMap a @> 
    member x.Item (arg:'c) = arg |> x.EvAMap |> x.EvMap
    //member x.Item(value:Term<'c>) =
    ///    let v = subst_var_value amaparg value.Expr amapbody in
     //   subst_var_value x.Arg v x.Body |> expand_as<'b> |> Term

    interface ISymbolic<Function<'a, 'b, 'c>, 'b> with
           member a.Expr = a.Body
           member a.Mutate(b:Expr<'b>) = 
               let map = expand_as<'a->'b> (recombine_func a.Vars b)
               Function(a.Domain, a.CoDomain, map, a.AMap)

    override x.ToString() = src x.Map

type Function<'a, 'b when 'a : equality and 'b: equality>(domain:ISet<'a>, codomain:ISet<'b>, map: Expr<'a->'b>) = inherit Function<'a, 'b, 'a>(domain, codomain, map, <@ id @>)

type Predicate<'a, 'c when 'a : equality and 'c: equality> = Function<'a, bool, 'c>

type Predicate<'a when 'a : equality> = Function<'a, bool>