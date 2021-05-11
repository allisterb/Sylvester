namespace Sylvester

open Sylvester
open Arithmetic
open N10
open Sylvester.Collections

/// Set of elements closed under some left-associative operation.
type ISemigroup<'t when 't: equality> =
    inherit IGroupoid<'t>

type Semigroup<'t when 't: equality>(set:ISet<'t>, op:BinaryOp<'t>) =
    inherit Groupoid<'t>(set, op)
    do op |> fail_if_not_left_associative
    interface ISemigroup<'t>
    
/// Set of elements closed under some left-associative commutative operation.
type CommutativeSemigroup<'t when 't: equality>(set:ISet<'t>, op:BinaryOp<'t>) =
    inherit Semigroup<'t>(set, op)
    do op |> fail_if_not_commutative

    /// A lattice can also be viewed as consisting of two commutative semigroups on the same set.
    //static member toLattice (set: ISet<'t>, join: CommutativeSemigroup<'t>, meet: CommutativeSemigroup<'t>) = Lattice(set, join.Op, meet.Op)

/// Category of semigroups with an n structure-preserving morphism.
type Semigroups<'ut, 'vt, 'n when 'ut : equality and 'vt: equality and 'n :> Number>(l:Semigroup<'ut>, r:Semigroup<'vt>, maps: Array<'n, Map<'ut, 'vt>>) = 
    inherit Category<'ut, 'vt, ``1``, ``1``, 'n>(l, r, maps)

