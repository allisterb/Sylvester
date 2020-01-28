namespace Sylvester

type IGroup<'U when 'U: equality> = 
    inherit IMonoid<'U> 
    abstract member Inverse: ('U->'U)

/// Set of elements closed under some left-associative operation with identity and an inverse unary operation.
type Group<'U when 'U: equality>(set:Set<'U>, op:'U->'U->'U, id:'U, inv: 'U -> 'U) =
    inherit Monoid<'U>(set, op, id)
    member val Inverse = inv
    interface IGroup<'U> with member val Inverse = inv

type AbelianGroup<'U when 'U: equality>(set:Set<'U>, op:'U->'U->'U, id:'U, inv: 'U -> 'U) =
    inherit Group<'U>(set, op, id, inv)
    do if not(Op.isCommutative op) then failwith ""

/// Category of groups with a structure-preserving morphism.
type Grp<'U when 'U : equality> = Category<'U, Group<'U>, card.one>