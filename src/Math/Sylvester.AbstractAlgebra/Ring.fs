namespace Sylvester

open System.Collections

open Sylvester.Arithmetic.N10
open Sylvester.Collections

/// Set of elements closed under a left-associative commutative operation and a 2nd left-associative distributive operation.
type IRing<'t when 't: equality> =  
    inherit IGroup<'t>
    abstract member Op2:BinaryOp<'t>

/// Set of elements closed under a left-associative commutative operations and a 2nd left-associative distributive operation.
type Ring<'t when 't: equality>(set:Set<'t>, group: AbelianGroup<'t>, monoid: Monoid<'t>) =
    inherit Struct<'t, card.two>(set, arrayOf2 (group.Ops.[zero]) (monoid.Ops.[zero]))
    do monoid.Op |> failIfNotDistributiveOver group.Op
    member val Op = group.Op
    member val Op2 = monoid.Op
    member val Group = group
    member val Monoid = monoid
    interface IRing<'t> with
        member val Set = set
        member val Op = group.Op
        member x.GetEnumerator(): Generic.IEnumerator<'t * 't * 't> = (let s = x.Set :> Generic.IEnumerable<'t> in s |> Seq.pairwise |> Seq.map (fun(a, b) -> (a, b, (group.Op) a b))).GetEnumerator()
        member x.GetEnumerator(): IEnumerator = (x :> Generic.IEnumerable<'t * 't * 't>).GetEnumerator () :> IEnumerator
        member val Identity = group.Identity
        member val Inverse = group.Inverse
        member val Op2 = monoid.Op

type CommutativeRing<'t when 't: equality>(set:Set<'t>, group: AbelianGroup<'t>, Monoid: CommutativeMonoid<'t>) =
    inherit Ring<'t>(set, group, Monoid)

[<AutoOpen>]
module Ring =
    let inline AdditiveRing<'t when 't : equality and 't : (static member Zero:'t) and 't: (static member (+) :'t -> 't -> 't) and 't: (static member (~-) :'t -> 't)>(set:Set<'t>, op, id) =
        Ring(set, AdditiveGroup(set), Monoid(set, op, id))

    /// Define a ring over a set which has +, *, operators and 0, 1 elements defined. 
    let inline IntegerRing<'t when 't : equality and 't : (static member Zero:'t) and 't : (static member One:'t) and 't: (static member (+) :'t -> 't -> 't) and 't: (static member (*) :'t -> 't -> 't) and 't: (static member (~-) :'t -> 't)> 
        (set: Set<'t>) =
        CommutativeRing(set, AdditiveGroup(set), MultiplicativeMonoid(set))

    /// Ring of 32-bit integers.
    let Integers = IntegerRing(infiniteSeq id)

   
