namespace Sylvester

open System.Collections

type Subsets<'t when 't : equality > = Subsets of Set<'t> * Set<Set<'t>>
with
    member x.ParentSet = 
        let (Subsets(set, _)) = x in set
    member x.Set = 
        let (Subsets(_, subsets)) = x in subsets
    member x.FailIfDoesNotContainParent() = if not(x.Set.HasElement x.ParentSet) then failwith "This set of subsets does not contain the parent set as a distinguished element."
    member x.FailIfDoesNotContainDistinguishedEmpty() = if not(x.Set.HasElement Empty) then failwith "This set of subsets does not contain the empty set as a distinguished element."
    interface ITotalOrder<Set<'t>> with
        member x.Set = x.Set
        member x.Order = (<)
    interface Generic.IEnumerable<Set<'t>> with
        member x.GetEnumerator(): Generic.IEnumerator<Set<'t>> = 
            (let s = x.Set :> Generic.IEnumerable<Set<'t>> in s |> Seq.sort).GetEnumerator()
        member x.GetEnumerator(): IEnumerator = let s = x.Set :> Generic.IEnumerable<Set<'t>> in s.GetEnumerator() :> IEnumerator        

module Subsets =
    type Set<'t when 't : equality> 
    with 
        member x.Subsets(f) = Subsets(x, x.Powerset |> Seq.filter f |> Set.toSet)

type SetAlgebra<'t when 't: equality>(subsets: Subsets<'t>, least: Set<'t>, greatest: Set<'t>) =
    inherit BooleanAlgebra<Set<'t>>(subsets, (|+|), (|*|), least, greatest, subsets.ParentSet.Complement)
    member x.Subsets = subsets
    member x.AsLattice = x :> IComplementedLattice<Set<'t>>        
    interface ITotalOrder<Set<'t>>
    interface Generic.IEnumerable<Set<'t>> with
        member x.GetEnumerator(): Generic.IEnumerator<Set<'t>> = 
            (let s = x.Set :> Generic.IEnumerable<Set<'t>> in s |> Seq.sort).GetEnumerator()
        member x.GetEnumerator(): IEnumerator = let s = x.Set :> Generic.IEnumerable<Set<'t>> in s.GetEnumerator() :> IEnumerator        
    new(subsets:Subsets<'t>) = SetAlgebra(subsets, Seq.min subsets, Seq.max subsets)
    new(set: Set<'t>) = SetAlgebra(Subsets(set, set.Powerset)) 

type SigmaAlgebra<'t when 't: equality>(subsets: Subsets<'t>) =
    inherit SetAlgebra<'t>(subsets)
    do subsets.FailIfDoesNotContainParent()
    do subsets.FailIfDoesNotContainDistinguishedEmpty()
