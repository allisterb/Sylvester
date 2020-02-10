namespace Sylvester

open System
open System.Collections

open FSharp.Quotations
open FSharp.Quotations.Patterns
open Sylvester.Arithmetic
open Sylvester.Collections

type IReflexiveRelation = interface end

type ISymmetricRelation = interface end

type IAntiSymmetricRelation = interface end

type ITransitiveRelation = interface end

type IEquivalenceRelation =
    inherit IReflexiveRelation
    inherit ISymmetricRelation
    inherit ITransitiveRelation

type Relation<'t when 't : equality>(set: Set<'t>, [<ReflectedDefinition(true)>] pred:Expr<LogicalPredicate<'t * 't>>) =
    let v,t,e = match pred with | WithValue(v, t, e) -> v,t,e | _ -> failwith "Unexpected expression."
    member val ParentSet = set
    member val Pred = v :?> LogicalPredicate<'t *'t>
    member val Expr = e
    member val ExprString = exprToString e
    interface ISetBuilder<'t * 't> with
        member val Pred = v :?> LogicalPredicate<'t * 't>
        member val Expr = e
        member val ExprString = exprToString e
    member x.Set = set.Prod.Subset(x.Pred)
    interface ISet<'t * 't> with member x.Set = x.Set
    interface Generic.IEnumerable<'t * 't> with
        member x.GetEnumerator(): Generic.IEnumerator<'t * 't> =
            let s = x.Set :> Generic.IEnumerable<'t *'t> in s.GetEnumerator()
        member x.GetEnumerator(): IEnumerator = (x.Set :> Generic.IEnumerable<'t * 't>).GetEnumerator () :> IEnumerator
    
    
[<AutoOpen>]
module Relation = 
    type Set<'t when 't : equality> with
        member x.Rel(f: LogicalPredicate<'t * 't>) = Relation(x, f)
