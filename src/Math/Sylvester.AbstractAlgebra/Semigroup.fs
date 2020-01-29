namespace Sylvester

/// Set of elements closed under some left-associative operation.
type Semigroup<'t when 't: equality>(set:Set<'t>, op:BinaryOp<'t>) =
    inherit Groupoid<'t>(set, op)
    do failIfNotLeftAssociative op
    
/// Set of elements closed under some left-associative commutative operation.
type CommutativeSemigroup<'t when 't: equality>(set:Set<'t>, op:BinaryOp<'t>) =
    inherit Semigroup<'t>(set, op)
    do failIfNotCommutative op

/// Category of semigroups with a structure-preserving morphism.
type Semigroups<'t when 't : equality>(l:Semigroup<'t>, r:Semigroup<'t>, map:Map<'t>) = inherit Category<'t, Semigroup<'t>, card.one>(Morph(l,r,map))

