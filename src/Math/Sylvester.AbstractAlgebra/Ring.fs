namespace Sylvester

open Sylvester.Arithmetic.N10
open Sylvester.Collections

/// Set of elements closed under a left-associative commutative operation and a 2nd left-associative distributive operation.
type IRing<'U when 'U: equality> = 
    inherit IStruct<'U, card.two> 
    abstract member Group: AbelianGroup<'U>
    abstract member Monoid: Monoid<'U>

/// Set of elements closed under a left-associative commutative operations and a 2nd left-associative distributive operation.
type Ring<'U when 'U: equality>(set:Set<'U>, group: AbelianGroup<'U>, monoid: Monoid<'U>) =
    inherit Struct<'U, card.two>(set, arrayOf2 (group.Ops.[zero]) (monoid.Ops.[zero]))
    do monoid.Op |> failIfNotDistributiveOver group.Op
    member val Op1 = group.Op
    member val Op2 = monoid.Op
    member val Group = group
    member val Monoid = monoid

    interface IRing<'U> with
        member val Group = group
        member val Monoid = monoid

type CommutativeRing<'U when 'U: equality>(set:Set<'U>, group: AbelianGroup<'U>, Monoid: CommutativeMonoid<'U>) =
    inherit Ring<'U>(set, group, Monoid)

[<AutoOpen>]
module Ring =
    let inline AdditiveRing<'U when 'U : equality and 'U : (static member Zero:'U) and 'U: (static member (+) :'U -> 'U -> 'U) and 'U: (static member (~-) :'U -> 'U)>(set:Set<'U>, op, id) =
        Ring(set, AdditiveGroup(set), Monoid(set, op, id))

    /// Define a ring over a set which has +, *, operators and 0, 1 elements defined. 
    let inline IntegerRing<'U when 'U : equality and 'U : (static member Zero:'U) and 'U : (static member One:'U) and 'U: (static member (+) :'U -> 'U -> 'U) and 'U: (static member (*) :'U -> 'U -> 'U) and 'U: (static member (~-) :'U -> 'U)> 
        (set: Set<'U>) =
        CommutativeRing(set, AdditiveGroup(set), MultiplicativeMonoid(set))

    /// Ring of 32-bit integers.
    let Integers = IntegerRing(infiniteSeq (fun n -> n))

    /// Ring of big integers.
    let BigIntegers = IntegerRing(BigInt)    
