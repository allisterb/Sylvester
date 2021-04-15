namespace Sylvester

open System.Collections

open Sylvester.Arithmetic
open Sylvester.Collections

/// Set of elements closed under a left-associative commutative invertible operation with identity, 
/// and a 2nd left-associative operation which distributes over the first operation.
type IRing<'t when 't: equality> =  
    inherit IAbelianGroup<'t>
    abstract member Op2:BinaryOp<'t>

/// Set of elements closed under a left-associative commutative operations and a 2nd left-associative distributive operation.
type Ring<'t when 't: equality>(group: IAbelianGroup<'t>, op2: BinaryOp<'t>) =
    inherit Struct<'t, card.four>(group.Set, arrayOf4 (Binary(group.Op)) (Nullary(group.Identity)) (Unary(group.Inverse)) (Binary(op2)))
    do fail_if_not_distributive_over op2 group.Op
    member val Op = group.Op
    member val Op2 = op2
    member val Group = group
    interface IRing<'t> with
        member val Set = group.Set
        member val Op = group.Op
        member val Op2 = op2
        member val Identity = group.Identity
        member val Inverse = group.Inverse        
        member x.GetEnumerator(): Generic.IEnumerator<'t * 't * 't> = (let s = x.Set :> Generic.IEnumerable<'t> in s |> Seq.pairwise |> Seq.map (fun(a, b) -> (a, b, (group.Op) a b))).GetEnumerator()
        member x.GetEnumerator(): IEnumerator = (x :> Generic.IEnumerable<'t * 't * 't>).GetEnumerator () :> IEnumerator
    new (set:ISet<'t>, op: BinaryOp<'t>, ident:'t, inv:UnaryOp<'t>, op2: BinaryOp<'t>) =
        Ring(AbelianGroup(set, op, ident, inv), op2)

/// Ring where the 2nd operation is commutative.
type CommutativeRing<'t when 't: equality>(group: IAbelianGroup<'t>, op2: BinaryOp<'t>) =
    inherit Ring<'t>(group, op2)
    do fail_if_not_commutative op2
    new (set:ISet<'t>, op: BinaryOp<'t>, op2: BinaryOp<'t>, zero:'t, one:'t, inv:UnaryOp<'t>) =
        CommutativeRing(AbelianGroup(set, op, zero, inv), op2)

/// Commutative ring with a total order relation.
type OrderedRing<'t when 't: equality and 't : comparison>(group: IAbelianGroup<'t>, op2: BinaryOp<'t>, order: Order<'t>) =
    inherit CommutativeRing<'t>(group, op2)
    interface ITotalOrder<'t> with
        member val Set = group.Set
        member val Order = order

type IIntegralDomain = interface end

type IIdeal<'t when 't : equality> =
    inherit IGroup<'t>
    abstract Op:BinaryOp<'t>

[<AutoOpen>]
module Ring =
    /// Zero ring.
    [<Formula>]
    let Zero = CommutativeRing(Group.Zero, (+))
    /// Additive ring.
    let inline AdditiveRing<'t when 't : equality and 't : (static member Zero:'t) and 't: (static member (+) :'t -> 't -> 't) and 't: (static member (~-) :'t -> 't)>(set:ISet<'t>, op2) =
        Ring(AbelianGroup(set, Binary(+).DestructureBinary, LanguagePrimitives.GenericZero, (~-)), op2)

    /// Ring of positive integers.
    let Zpos =
        let set = infinite_seq' id 
        let order = (<=) in
        {
            new OrderedRing<int>(AdditiveGroup(set), (*), order) 
                interface ITotalOrder<int> with
                    member x.Set = set
                    member x.Order = order
                interface IHasLeast<int> with 
                    member x.Least = 0
                    member x.Minimal = 0
                    member x.LowerBound = 0
                interface IWellOrder<int> with
                    member x.Least(subset:Set<int>) = subset |> Seq.sortWith (fun a b -> (if order a b then -1 else 1)) |> Seq.item 0
                interface Generic.IEnumerable<int> with
                    member x.GetEnumerator(): Generic.IEnumerator<int> = (set :> Generic.IEnumerable<int>).GetEnumerator()
                    member x.GetEnumerator(): IEnumerator = (set :> IEnumerable).GetEnumerator()    
        }

    /// Ring of negative integers.
    let Zneg =
        let set = infinite_seq' (fun n -> -n) 
        let order = (<=) in
        {
            new OrderedRing<int>(AdditiveGroup(set), (*), order)
                interface ITotalOrder<int> with
                    member x.Set = set
                    member x.Order = order
                interface IHasGreatest<int> with 
                    member x.Greatest = 0
                    member x.Maximal = 0
                    member x.UpperBound = 0
                interface IWellOrder<int> with
                    member x.Least(subset:Set<int>) = subset |> Seq.sort |> Seq.item 0
                interface Generic.IEnumerable<int> with
                    member x.GetEnumerator(): Generic.IEnumerator<int> = (set :> Generic.IEnumerable<int>).GetEnumerator()
                    member x.GetEnumerator(): IEnumerator = (set :> IEnumerable).GetEnumerator()
        }

    /// Ring of integers.
    let Z =
        let set = Zpos |+| Zneg
        let order = (<=) in
        {
            new OrderedRing<int>(AdditiveGroup(set), (*), order) 
                interface ITotalOrder<int> with
                    member x.Set = set
                    member x.Order = order
                interface IWellOrder<int> with
                    member x.Least(subset:Set<int>) = subset |> Seq.sort |> Seq.item 0
                interface Generic.IEnumerable<int> with
                    member x.GetEnumerator(): Generic.IEnumerator<int> = (set :> Generic.IEnumerable<int>).GetEnumerator()
                    member x.GetEnumerator(): IEnumerator = (set :> IEnumerable).GetEnumerator()     
        }

    /// Ring of natural numbers with zero.
    let N = Zpos
    
    /// Ring of natural numbers without zero.
    let Nz =
        let set = Zpos |^| 0
        let order = (<=) in
        {
            new OrderedRing<int>(AdditiveGroup(set), (*), order) 
                interface IWellOrder<int> with
                    member x.Least(subset:Set<int>) = subset |> Seq.sort |> Seq.item 0
                interface ITotalOrder<int> with
                    member x.Set = set
                    member x.Order = order
                interface IHasLeast<int> with 
                    member x.Least = 0
                    member x.Minimal = 0
                    member x.LowerBound = 0
                interface Generic.IEnumerable<int> with
                    member x.GetEnumerator(): Generic.IEnumerator<int> = (set :> Generic.IEnumerable<int>).GetEnumerator()
                    member x.GetEnumerator(): IEnumerator = (set :> IEnumerable).GetEnumerator()     
        }
    
    let Z1 = CommutativeRing(Z, Mod.(+) 1, Mod.(*) 1, 0, 1, (~-))
    let Z2 = CommutativeRing(Z, Mod.(+) 2, Mod.(*) 2, 0, 1, (~-))
    let Z3 = CommutativeRing(Z, Mod.(+) 3, Mod.(*) 3, 0, 1, (~-))
    let Z4 = CommutativeRing(Z, Mod.(+) 4, Mod.(*) 4, 0, 1, (~-))
    let Z5 = CommutativeRing(Z, Mod.(+) 5, Mod.(*) 5, 0, 1, (~-))
    let Z6 = CommutativeRing(Z, Mod.(+) 6, Mod.(*) 6, 0, 1, (~-))
    let Z7 = CommutativeRing(Z, Mod.(+) 7, Mod.(*) 7, 0, 1, (~-))
    let Z8 = CommutativeRing(Z, Mod.(+) 8, Mod.(*) 8, 0, 1, (~-))
    let Z9 = CommutativeRing(Z, Mod.(+) 9, Mod.(*) 9, 0, 1, (~-))