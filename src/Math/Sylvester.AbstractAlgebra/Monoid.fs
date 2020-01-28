namespace Sylvester

type IMonoid<'U when 'U: equality> = 
    inherit IGroupoid<'U>
    abstract member Identity: 'U

/// Set of elements closed under some left-associative operation with identity.
type Monoid<'U when 'U: equality>(set:Set<'U>, op:'U->'U->'U, id:'U) =
    inherit Groupoid<'U>(set, op)
    member val Identity = id
    interface IMonoid<'U> with member val Identity = id

/// Category of monoids with a structure-preserving morphism.
type Mon<'U when 'U : equality> = Category<'U, Monoid<'U>, card.one>