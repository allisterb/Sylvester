namespace Sylvester

/// Set of elements closed under some left-associative operation with identity and an inverse unary operation.
type Group<'U when 'U: equality>(set:Set<'U>, op:'U->'U->'U, id:'U, inv: 'U -> 'U) =
    inherit Groupoid<'U>(set, op)
    member x.Identity = id
    member x.Inverse = inv

/// Category of monoids with a structure-preserving morphism.
type Grp<'U when 'U : equality> = Category<'U, Monoid<'U>, Card.one>