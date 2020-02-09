namespace Sylvester

open System
open System.Collections

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

type Relation<'t when 't : equality>(set: Set<'t * 't>) =
    member x.Set = set
    new(set: Set<'t>, f:LogicalPredicate<'t * 't>) = 
        let p:Set<'t*'t> = (set * set) in Relation(p.Subset f)
    member x.IsReflexive = set |> Seq.forall (fun(a, _) -> set.HasElement (a, a))
    member x.IsSymmetric = set |> Seq.forall (fun(a, b) -> set.HasElement (b, a))
    member x.IsTransitive = 
        let mutable result = true
        for (a, b) in set do
            for (_b, c) in set |> Seq.filter (fun (r1, c) -> r1 = b) do
                if (set.HasElement (a,c)) |> not then 
                    result <- false
        result

[<AutoOpen>]
module Relation = 
    type Set<'t when 't : equality> with
        member x.Rel(f: LogicalPredicate<'t * 't>) = Relation(x, f)
