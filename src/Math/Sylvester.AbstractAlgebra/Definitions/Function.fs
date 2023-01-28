namespace Sylvester

open FSharp.Quotations

// A binary relation between 2 sets
type IRelation<'a, 'b, 'c when 'a : equality and 'b : equality and 'c:equality> = 
    abstract member Domain: Set<'a>
    abstract member CoDomain: Set<'b>
    abstract member Op:Expr<'a->'b->bool>

type IRelation<'a, 'b when 'a : equality and 'b : equality> = IRelation<'a, 'b, 'a>

type IRelation<'t when 't: equality> = IRelation<'t, 't>

type Function<'a, 'b, 'c when 'a : equality and 'b: equality and 'c: equality>(domain:Set<'a>, codomain:Set<'b>, op: Expr<'a->'b->bool>, amap:'c->'a) =
    member x.Domain = domain
    member x.CoDomain = codomain
    member x.Op = op
    member x.EvOp = evaluate op
    member x.AMap = amap
    interface IRelation<'a, 'b, 'c> with
        member x.Domain = x.Domain
        member x.CoDomain = x.CoDomain
        member x.Op = x.Op
    member x.Exec (arg:'c) = arg |> x.AMap |> x.EvOp

type Function<'a, 'b when 'a : equality and 'b: equality>(domain:Set<'a>, codomain:Set<'b>, op: Expr<'a->'b->bool>) = inherit Function<'a, 'b, 'a>(domain, codomain, op, id)

type Predicate<'a, 'c when 'a : equality and 'c: equality> = Function<'a, bool, 'c>

type Predicate<'a when 'a : equality> = Function<'a, bool>