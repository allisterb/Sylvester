namespace Sylvester

open System.Collections

open Sylvester.Arithmetic
open Sylvester.Collections

/// Set of elements closed under one operation that is associative, commutative, and idempotent, and for every pair of elements returns the infimum of that pair.
type ISemiLattice<'t when 't: equality> = 
    inherit ISet<'t>
    inherit Generic.IEnumerable<'t * 't * 't>
    abstract member Join: BinaryOp<'t>
    
/// Set of elements closed under one operation that is associative, commutative, and idempotent, and for every pair of elements returns the infimum of that pair.
type SemiLattice<'t when 't: equality>(set: ISet<'t>, join: BinaryOp<'t>) =
    inherit Struct<'t, card.one>(set, arrayOf1 (Binary(join)))
    do failIfNotLeftAssociative join
    do failIfNotCommutative join
    
/// Set of elements closed under two operations that are associative, commutative, and idempotent, and for every pair of elements returns the infimum and supremum respectively of that pair.
type ILattice<'t when 't: equality> = 
    inherit ISet<'t>
    inherit Generic.IEnumerable<'t * 't * 't>
    abstract member Join: BinaryOp<'t>
    abstract member Meet: BinaryOp<'t>

/// Set of elements closed under two operations that are associative, commutative, and idempotent, and for every pair of elements returns the infimum and supremum respectively of that pair..
type Lattice<'t when 't: equality>(set: ISet<'t>, join: BinaryOp<'t>, meet: BinaryOp<'t>) =
    inherit Struct<'t, card.two>(set, arrayOf2 (Binary(join)) (Binary(meet)))
    do failIfNotLeftAssociative join
    do failIfNotCommutative join
    do failIfNotLeftAssociative meet
    do failIfNotCommutative meet