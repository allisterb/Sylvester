namespace Sylvester

open System
open System.Collections

type SubSets<'t when 't : equality > = SubSets of Set<'t> * Set<Set<'t>>
with
    member x.ParentSet = 
        let (SubSets(set, _)) = x in set
    member x.Set = 
        let (SubSets(_, subsets)) = x in subsets
    interface ITotalOrder<Set<'t>> with
        member x.Set = x.Set
        member x.Order = (<)
    interface Generic.IEnumerable<Set<'t>> with
        member x.GetEnumerator(): Generic.IEnumerator<Set<'t>> = 
            (let s = x.Set :> Generic.IEnumerable<Set<'t>> in s |> Seq.sort).GetEnumerator()
        member x.GetEnumerator(): IEnumerator = let s = x.Set :> Generic.IEnumerable<Set<'t>> in s.GetEnumerator() :> IEnumerator        

type SetAlgebra<'t when 't: equality>(subsets: SubSets<'t>) =
    inherit BooleanAlgebra<Set<'t>>(subsets, (|+|), (|*|), Empty, subsets.ParentSet, subsets.ParentSet.Complement)
    new(set: Set<'t>) = SetAlgebra(SubSets(set, set.Powerset))
    interface ITotalOrder<Set<'t>>
    interface Generic.IEnumerable<Set<'t>> with
        member x.GetEnumerator(): Generic.IEnumerator<Set<'t>> = 
            (let s = x.Set :> Generic.IEnumerable<Set<'t>> in s |> Seq.sort).GetEnumerator()
        member x.GetEnumerator(): IEnumerator = let s = x.Set :> Generic.IEnumerable<Set<'t>> in s.GetEnumerator() :> IEnumerator        

type SigmaAlgebra<'t when 't: equality>(subsets: SubSets<'t>) =
    inherit SetAlgebra<'t>(subsets)