namespace Sylvester

open FSharp.Quotations

// A binary relation between 2 sets
type IRelation<'a, 'b when 'a : equality and 'b : equality> = 
    abstract member Domain: Set<'a>
    abstract member CoDomain: Set<'b>
    abstract member Op:Expr<'a->'b->bool>

type IReflexiveRelation<'a, 'b when 'a : equality and 'b : equality>  = 
    inherit IRelation<'a, 'b>

type ISymmetricRelation<'a, 'b when 'a : equality and 'b : equality>  = 
    inherit IRelation<'a, 'b>

type IAntiSymmetricRelation<'a, 'b when 'a : equality and 'b : equality>  = 
    inherit IRelation<'a, 'b>

type ITransitiveRelation<'a, 'b when 'a : equality and 'b : equality>  = 
    inherit IRelation<'a, 'b>

type IEquivalenceRelation<'a, 'b when 'a : equality and 'b : equality> =
    inherit IReflexiveRelation<'a, 'b>
    inherit ISymmetricRelation<'a, 'b>
    inherit ITransitiveRelation<'a, 'b>

type IRelation<'t when 't: equality> = IRelation<'t, 't>

type IReflexiveRelation<'t when 't : equality> = IReflexiveRelation<'t, 't>

type ISymmetricRelation<'t when 't : equality> = ISymmetricRelation<'t, 't>
    
type IAntiSymmetricRelation<'t when 't : equality> = IAntiSymmetricRelation<'t, 't>

type ITransitiveRelation<'t when 't : equality> = ITransitiveRelation<'t, 't>

type Function<'a, 'b when 'a : equality and 'b: equality> =
| Fn of Set<'a> * Set<'b> * Expr<'a->'b->bool> with
    member x.Domain = let (Fn(domain, _, _)) = x in domain
    member x.CoDomain = let (Fn(_, codomain, _)) = x in codomain
    member x.Op = let (Fn(_, _, op)) = x in op
    interface IRelation<'a, 'b> with
        member x.Domain = x.Domain
        member x.CoDomain = x.CoDomain
        member x.Op = x.Op