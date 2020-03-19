namespace Sylvester

open Sylvester.Arithmetic
open Sylvester.Collections

/// Set of elements closed under some left-associative operation.
type ISemigroup<'t when 't: equality> =
    inherit IGroupoid<'t>

type Semigroup<'t when 't: equality>(set:ISet<'t>, op:BinaryOp<'t>) =
    inherit Groupoid<'t>(set, op)
    do failIfNotLeftAssociative op
    interface ISemigroup<'t>
    
/// Set of elements closed under some left-associative commutative operation.
type CommutativeSemigroup<'t when 't: equality>(set:ISet<'t>, op:BinaryOp<'t>) =
    inherit Semigroup<'t>(set, op)
    do failIfNotCommutative op

/// Category of semigroups with an n structure-preserving morphism.
type Semigroups<'ut, 'vt, 'n when 'ut : equality and 'vt: equality and 'n :> Number>(l:Semigroup<'ut>, r:Semigroup<'vt>, maps: Array<'n, Map<'ut, 'vt>>) = 
    inherit Category<'ut, 'vt, card.one, card.one, 'n>(l, r, maps)

