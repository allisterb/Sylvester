namespace Sylvester

open Sylvester.Arithmetic.N10
open Sylvester.Collections

type IRing<'U when 'U: equality> = 
    inherit IStruct<'U, card.two> 
    abstract member Group: AbelianGroup<'U>
    abstract member Monoid: Monoid<'U>

/// Set of elements closed under a left-associative commutative operations and a distributive operation.
type Ring<'U when 'U: equality>(set:Set<'U>, group: AbelianGroup<'U>, monoid: Monoid<'U>) =
    inherit Struct<'U, card.two>(set, arrayOf2 (group.Ops.[zero]) (monoid.Ops.[zero]))
    do monoid.Op |> failIfNotDistributiveOver group.Op
    member val Op1 = group.Op
    member val Op2 = monoid.Op
    interface IRing<'U> with
        member val Group = group
        member val Monoid = monoid

    
type CommutativeRing<'U when 'U: equality>(set:Set<'U>, group: AbelianGroup<'U>, monoid: CommutativeMonoid<'U>) =
    inherit Ring<'U>(set, group, monoid)

[<AutoOpen>]
module Ring =
    /// Define a ring over a set which has +, *, 0. 
    let inline IntegerRing<'U when 'U : equality and 'U : (static member Zero:'U) and 'U : (static member One:'U) and 'U: (static member (+) :'U -> 'U -> 'U) and 'U: (static member (*) :'U -> 'U -> 'U) and 'U: (static member (~-) :'U -> 'U)> 
        (set: Set<'U>) =
        CommutativeRing(set, AdditiveGroup(set), MultiplicativeMonoid(set))

    let Integers = IntegerRing(Int)

        
