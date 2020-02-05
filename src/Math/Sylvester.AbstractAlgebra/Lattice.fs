namespace Sylvester

open System.Collections

open Sylvester.Collections

/// Set of elements closed under a operation that is associative, commutative, and idempotent, and for every pair of elements returns the infimum of that pair
/// which induces a partial order on the set.
type ISemiLattice<'t when 't: equality> = 
    inherit IStruct<'t, card.one>
    inherit IPartialOrder<'t>
    abstract member Join: BinaryOp<'t>
    
/// Set of elements closed under a operation that is associative, commutative, and idempotent, and for every pair of elements returns the infimum of that pair
/// which induces a partial order on the set.
type SemiLattice<'t when 't: equality>(set: ISet<'t>, join: BinaryOp<'t>, order: Order<'t>) =
    inherit Struct<'t, card.one>(set, arrayOf1 (Binary(join)))
    do failIfNotLeftAssociative join
    do failIfNotCommutative join
    interface ISemiLattice<'t> with
        member val Set = set.Set
        member val Order = order
        member x.GetEnumerator(): Generic.IEnumerator<'t * 't * bool> = 
            (let s = x.Set :> Generic.IEnumerable<'t> in s |> Seq.pairwise |> Seq.map (fun(a, b) -> (a, b, (order) a b))).GetEnumerator()
        member x.GetEnumerator(): IEnumerator = (x :> Generic.IEnumerable<'t * 't * bool>).GetEnumerator () :> IEnumerator
        member x.Join = join    
    
/// Set of elements closed under two operations that are associative, commutative, and idempotent, and for every pair of elements returns the infimum and supremum respectively of that pair.
type ILattice<'t when 't: equality> = 
    inherit IStruct<'t, card.two>
    inherit IPartialOrder<'t>
    abstract member Join: BinaryOp<'t>
    abstract member Meet: BinaryOp<'t>

/// Set of elements closed under two operations that are associative, commutative, and idempotent, and for every pair of elements returns the infimum and supremum respectively of that pair..
type Lattice<'t when 't: equality>(set: ISet<'t>, join: BinaryOp<'t>, meet: BinaryOp<'t>, order: Order<'t>) =
    inherit Struct<'t, card.two>(set, arrayOf2 (Binary(join)) (Binary(meet)))
    do failIfNotLeftAssociative join
    do failIfNotCommutative join
    do failIfNotLeftAssociative meet
    do failIfNotCommutative meet
    interface ILattice<'t> with
        member val Set = set.Set
        member val Order = order
        member x.GetEnumerator(): Generic.IEnumerator<'t * 't * bool> = 
            (let s = x.Set :> Generic.IEnumerable<'t> in s |> Seq.pairwise |> Seq.map (fun(a, b) -> (a, b, (order) a b))).GetEnumerator()
        member x.GetEnumerator(): IEnumerator = (x :> Generic.IEnumerable<'t * 't * bool>).GetEnumerator () :> IEnumerator
        member x.Join = join 
        member x.Meet = meet
    
