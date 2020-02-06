namespace Sylvester

open System.Collections

open Sylvester.Collections

/// Set of elements closed under a operation that is associative, commutative, and idempotent, which induces a partial order on the set 
/// such that the operation on every pair of elements results in the supremum of the pair.
type ISemiLattice<'t when 't: equality> = 
    inherit IStruct<'t, card.one>
    inherit IPartialOrder<'t>
    abstract member Op: BinaryOp<'t>
    
/// Set of elements closed under a operation that is associative, commutative, and idempotent, which induces a partial order on the set 
/// such that the operation on every pair of elements results in the supremum of the pair.
type SemiLattice<'t when 't: equality>(set: ISet<'t>, op: BinaryOp<'t>, order: Order<'t>) =
    inherit Struct<'t, card.one>(set, arrayOf1 (Binary(op)))
    do failIfNotLeftAssociative op
    do failIfNotCommutative op
    do failIfNotIdempotent op
    interface ISemiLattice<'t> with
        member val Set = set.Set
        member val Op = op
        member val Order = order
        member x.GetEnumerator(): Generic.IEnumerator<'t> = 
            (let s = x.Set :> Generic.IEnumerable<'t> in s |> Seq.sortWith (fun a b -> (if order a b then -1 else 1))).GetEnumerator()
        member x.GetEnumerator(): IEnumerator = (let s = x.Set :> Generic.IEnumerable<'t> in s |> Seq.sortWith (fun a b -> if order a b then -1 else 1)).GetEnumerator() :> IEnumerator
            
type IBoundedJoinSemiLattice<'t when 't : equality> =
    inherit ISemiLattice<'t>
    inherit IHasLeast<'t>

type IBoundedMeetSemiLattice<'t when 't : equality> =
    inherit ISemiLattice<'t>
    inherit IHasGreatest<'t>

/// Set of elements closed under 2 operations that are associative, commutative, and idempotent, which induces a partial order on the set 
/// such that each operation on every pair of elements results in the supremum and infimum respectively of the pair.
type ILattice<'t when 't: equality> = 
    inherit IStruct<'t, card.two>
    inherit IPartialOrder<'t>
    abstract member Join: BinaryOp<'t>
    abstract member Meet: BinaryOp<'t>


/// Set of elements closed under 2 operations that are associative, commutative, and idempotent, which induces a partial order on the set 
/// such that each operation on every pair of elements results in the supremum and infimum respectively of the pair.
type Lattice<'t when 't: equality>(set: ISet<'t>, join: BinaryOp<'t>, meet: BinaryOp<'t>) =
    inherit Struct<'t, card.two>(set, arrayOf2 (Binary(join)) (Binary(meet)))
    do failIfNotLeftAssociative join
    do failIfNotCommutative join
    do failIfNotLeftAssociative meet
    do failIfNotCommutative meet
    let order = (fun a b -> (if (join a b) = a then false else true))
    interface ILattice<'t> with
        member val Set = set.Set
        member val Order = order
        member x.GetEnumerator(): Generic.IEnumerator<'t> = 
            (let s = x.Set :> Generic.IEnumerable<'t> in s |> Seq.sortWith (fun a b -> (if order a b then -1 else 1))).GetEnumerator()
        member x.GetEnumerator(): IEnumerator = (let s = x.Set :> Generic.IEnumerable<'t> in s |> Seq.sortWith (fun a b -> if order a b then -1 else 1)).GetEnumerator() :> IEnumerator        
        member x.Join = join 
        member x.Meet = meet
    /// A Lattice can also be viewed as consisting of two commutative semigroups on the same set.
    new (set: ISet<'t>, join: CommutativeSemigroup<'t>, meet: CommutativeSemigroup<'t>) = Lattice(set, join.Op, meet.Op)
    
type BoundedLattice<'t when 't: equality>(set: ISet<'t>, join: BinaryOp<'t>, meet: BinaryOp<'t>, greatest:'t, least:'t) =
    inherit Lattice<'t>(set, join, meet)
    interface IHasLeast<'t> with 
        member val LowerBound = least
        member val Minimal = least
        member val Least = least
    interface IHasGreatest<'t> with 
        member val UpperBound = greatest
        member val Maximal = greatest
        member val Greatest = greatest

    new (set: ISet<'t>, join: IBoundedJoinSemiLattice<'t>, meet: IBoundedMeetSemiLattice<'t>) = 
        BoundedLattice(set, join.Op, meet.Op, join.Least, meet.Greatest)