namespace Sylvester

open Sylvester.Arithmetic.N10
open Sylvester.Collections

/// Set of elements closed under a left-associative commutative operation and a 2nd left-associative distributive operation.
type IRing<'t when 't: equality> = 
    inherit IStruct<'t, card.two> 
    abstract member Group: AbelianGroup<'t>
    abstract member Monoid: Monoid<'t>

/// Set of elements closed under a left-associative commutative operations and a 2nd left-associative distributive operation.
type Ring<'t when 't: equality>(set:Set<'t>, group: AbelianGroup<'t>, monoid: Monoid<'t>) =
    inherit Struct<'t, card.two>(set, arrayOf2 (group.Ops.[zero]) (monoid.Ops.[zero]))
    do monoid.Op |> failIfNotDistributiveOver group.Op
    member val Op1 = group.Op
    member val Op2 = monoid.Op
    member val Group = group
    member val Monoid = monoid

    interface IRing<'t> with
        member val Group = group
        member val Monoid = monoid

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

   
