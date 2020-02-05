namespace Sylvester

open System.Collections

open Sylvester.Arithmetic
open Sylvester.Collections

/// Set of elements closed under a left-associative commutative operation and a 2nd left-associative distributive operation.
type IRing<'t when 't: equality> =  
    inherit IGroup<'t>
    abstract member Op2:BinaryOp<'t>

/// Set of elements closed under a left-associative commutative operations and a 2nd left-associative distributive operation.
type Ring<'t when 't: equality>(group: AbelianGroup<'t>, monoid: Monoid<'t>) =
    inherit Struct<'t, card.two>(group.Set, arrayOf2 (group.Ops.[N10.zero]) (monoid.Ops.[N10.zero]))
    do monoid.Op |> failIfNotDistributiveOver group.Op
    member val Op = group.Op
    member val Op2 = monoid.Op
    member val Group = group
    member val Monoid = monoid
    interface IRing<'t> with
        member val Set = group.Set
        member val Op = group.Op
        member x.GetEnumerator(): Generic.IEnumerator<'t * 't * 't> = (let s = x.Set :> Generic.IEnumerable<'t> in s |> Seq.pairwise |> Seq.map (fun(a, b) -> (a, b, (group.Op) a b))).GetEnumerator()
        member x.GetEnumerator(): IEnumerator = (x :> Generic.IEnumerable<'t * 't * 't>).GetEnumerator () :> IEnumerator
        member val Identity = group.Identity
        member val Inverse = group.Inverse
        member val Op2 = monoid.Op

    new (set:ISet<'t>, op: BinaryOp<'t>, op2: BinaryOp<'t>, zero:'t, one:'t, inv:UnaryOp<'t>) =
        Ring(AbelianGroup(set, op, zero, inv), Monoid(set, op2, one))

type CommutativeRing<'t when 't: equality>(group: AbelianGroup<'t>, Monoid: CommutativeMonoid<'t>) =
    inherit Ring<'t>(group, Monoid)
    new (set:ISet<'t>, op: BinaryOp<'t>, op2: BinaryOp<'t>, zero:'t, one:'t, inv:UnaryOp<'t>) =
        CommutativeRing(AbelianGroup(set, op, zero, inv), CommutativeMonoid(set, op2, one))

[<AutoOpen>]
module Ring =
    let inline AdditiveRing<'t when 't : equality and 't : (static member Zero:'t) and 't: (static member (+) :'t -> 't -> 't) and 't: (static member (~-) :'t -> 't)>(set:ISet<'t>, op, id) =
        Ring(AdditiveGroup(set), Monoid(set, op, id))

    /// Define a ring over a set which has +, *, operators and 0, 1 elements defined. 
    let inline IntegerRing<'t when 't : equality and 't : (static member Zero:'t) and 't : (static member One:'t) and 't: (static member (+) :'t -> 't -> 't) and 't: (static member (*) :'t -> 't -> 't) and 't: (static member (~-) :'t -> 't)> 
        (set: ISet<'t>) =
        CommutativeRing(AdditiveGroup(set), MultiplicativeMonoid(set))