namespace Sylvester

/// Set of elements closed under a operation that is associative, commutative, and idempotent, which induces a partial order on the set 
/// such that the operation on every pair of elements results in the supremum of the pair.
type ISemiLattice<'t when 't: equality> = 
    inherit ISet<'t>
    inherit IPartialOrder<'t>
    abstract member Op: BinaryOp<'t>
    
/// Set of elements closed under a operation that is associative, commutative, and idempotent, which induces a partial order on the set 
/// such that the operation on every pair of elements results in the supremum of the pair.
type SemiLattice<'t when 't: equality>(set: ISet<'t>, op: BinaryOp<'t>) =
    inherit Poset<'t>(set, (fun a b -> (if (op a b) = a then false else true)))
    do 
        fail_if_not_left_associative op
        fail_if_not_commutative op
        fail_if_not_idempotent op
    interface ISemiLattice<'t> with
        member val Op = op
            
type IBoundedJoinSemiLattice<'t when 't : equality> =
    inherit ISemiLattice<'t>
    inherit ILeast<'t>

type IBoundedMeetSemiLattice<'t when 't : equality> =
    inherit ISemiLattice<'t>
    inherit IGreatest<'t>

/// Set of elements closed under 2 operations that are associative, commutative, and idempotent, which induces a partial order on the set 
/// such that each operation on every pair of elements results in the supremum and infimum respectively of the pair.
type ILattice<'t when 't: equality and 't: comparison> = 
    inherit ISet<'t>
    inherit ITotalOrder<'t>
    abstract Join: BinaryOp<'t>
    abstract Meet: BinaryOp<'t>

type IBoundedLattice<'t when 't : equality and 't: comparison> =
    inherit ILattice<'t>
    inherit IGreatest<'t>
    inherit ILeast<'t>

type IComplementedLattice<'t when 't : equality and 't: comparison> =
    inherit IBoundedLattice<'t>
    abstract Complement:UnaryOp<'t>

type IDistributedComplementedLattice<'t when 't : equality and 't: comparison> = inherit IComplementedLattice<'t>

/// Set of elements closed under 2 operations that are associative, commutative, and idempotent, which induces a partial order on the set 
/// such that each operation on every pair of elements results in the supremum and infimum respectively of the pair.
type Lattice<'t when 't: equality and 't: comparison>(set: ISet<'t>, join: BinaryOp<'t>, meet: BinaryOp<'t>) =
    inherit OrderedSet<'t>(set)
    do 
        fail_if_not_left_associative join
        fail_if_not_commutative join
        fail_if_not_idempotent join
        fail_if_not_left_associative meet
        fail_if_not_commutative meet
        fail_if_not_idempotent meet
    interface ILattice<'t> with
        member val Join = join 
        member val Meet = meet
    
type BoundedLattice<'t when 't: equality and 't: comparison>(set: ISet<'t>, join: BinaryOp<'t>, meet: BinaryOp<'t>, least:'t, greatest:'t) =
    inherit Lattice<'t>(set, join, meet)
    interface IBoundedLattice<'t> with 
        member val LowerBound = least
        member val Minimal = least
        member val Least = least 
        member val UpperBound = greatest
        member val Maximal = greatest
        member val Greatest = greatest

    new (set: ISet<'t>, join: IBoundedJoinSemiLattice<'t>, meet: IBoundedMeetSemiLattice<'t>) = 
        BoundedLattice(set, join.Op, meet.Op, join.Least, meet.Greatest)

type ComplementedLattice<'t when 't: equality and 't: comparison>(set: ISet<'t>, join: BinaryOp<'t>, meet: BinaryOp<'t>, least:'t, greatest:'t, complement:UnaryOp<'t>) =
    inherit BoundedLattice<'t>(set, join, meet, least, greatest)        
        member val Complement = complement
    interface IComplementedLattice<'t> with member val Complement = complement

type DistributedComplementedLattice<'t when 't: equality and 't: comparison>(set: ISet<'t>, join: BinaryOp<'t>, meet: BinaryOp<'t>, least:'t, greatest:'t, complement:UnaryOp<'t>) =
    inherit ComplementedLattice<'t>(set, join, meet, least, greatest, complement)
    do 
        fail_if_not_distributive_over join meet
        fail_if_not_distributive_over meet join