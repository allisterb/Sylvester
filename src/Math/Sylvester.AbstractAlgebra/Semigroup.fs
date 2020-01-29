namespace Sylvester

/// Set of elements closed under some left-associative operation.
type Semigroup<'U when 'U: equality>(set:Set<'U>, op:BinaryOp<'U>) =
    inherit Groupoid<'U>(set, op)
    do failIfNotLeftAssociative op
    
/// Set of elements closed under some left-associative commutative operation.
type CommutativeSemigroup<'U when 'U: equality>(set:Set<'U>, op:BinaryOp<'U>) =
    inherit Semigroup<'U>(set, op)
    do failIfNotCommutative op

/// Category of semigroups with a structure-preserving morphism.
type Semigroups<'U when 'U : equality>(l:Semigroup<'U>, r:Semigroup<'U>, map:Map<'U>) = inherit Category<'U, Semigroup<'U>, card.one>(Morph(l,r,map))

