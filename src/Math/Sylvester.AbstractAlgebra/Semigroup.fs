namespace Sylvester

open Sylvester.Collections
/// Set of elements closed under some left-associative operation.
type Semigroup<'t when 't: equality>(set:Set<'t>, op:BinaryOp<'t>) =
    inherit Groupoid<'t>(set, op)
    do failIfNotLeftAssociative op
    
/// Set of elements closed under some left-associative commutative operation.
type CommutativeSemigroup<'t when 't: equality>(set:Set<'t>, op:BinaryOp<'t>) =
    inherit Semigroup<'t>(set, op)
    do failIfNotCommutative op

/// Category of semigroups with a structure-preserving morphism.
type Semigroups<'ut, 'vt when 'ut : equality and 'vt: equality>(l:Semigroup<'ut>, r:Semigroup<'vt>, map: Map<'ut, 'vt>) = 
    inherit Category<'ut, 'vt, card.one, card.one, Semigroup<'ut>, Semigroup<'vt>, card.one>(Morph(l, r, map) |> arrayOf1)

