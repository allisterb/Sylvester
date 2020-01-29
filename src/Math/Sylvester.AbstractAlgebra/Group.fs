namespace Sylvester

open Sylvester.Collections

/// Set of elements closed under some left-associative operation with identity and an inverse unary operation.
type IGroup<'U when 'U: equality> = 
    inherit IMonoid<'U> 
    abstract member Inverse: UnaryOp<'U>

/// Set of elements closed under some left-associative operation with identity and an inverse unary operation.
type Group<'U when 'U: equality>(set:Set<'U>, op:BinaryOp<'U>, id:'U, inv: UnaryOp<'U>) =
    inherit Monoid<'U>(set, op, id)
    member val Inverse = inv
    interface IGroup<'U> with member val Inverse = inv

type AbelianGroup<'U when 'U: equality>(set:Set<'U>, op: BinaryOp<'U>, id:'U, inv: UnaryOp<'U>) =
    inherit Group<'U>(set, op, id, inv)
    do failIfNotCommutative op

/// Category of groups with a structure-preserving morphism.
type Grp<'U when 'U : equality>(l:Group<'U>, r:Group<'U>, map: Map<'U>) = inherit Category<'U, Group<'U>, card.one>(Morph(l, r, map))

[<AutoOpen>]
module Group =
    /// Define a group over a set which has an additive operator and zero and negation. 
    let inline AdditiveGroup<'U when 'U : equality and 'U : (static member Zero:'U) and 'U: (static member (+) :'U -> 'U -> 'U) and 'U: (static member (~-) :'U -> 'U)> 
        (set: Set<'U>) =
        let id = LanguagePrimitives.GenericZero<'U>
        AbelianGroup(set, Binary(+).DestructureBinary, id, (~-))

    /// Define a group over a set which has a multiplicative operator and one and division.
    let inline MultiplicativeGroup<'U when 'U : equality and 'U : (static member One:'U) and 'U: (static member (*) :'U -> 'U -> 'U) and 'U: (static member (/) :'U -> 'U -> 'U)>
        (set: Set<'U>) =
        let one = LanguagePrimitives.GenericOne<'U>
        AbelianGroup(set, FSharpPlus.Math.Generic.(*), one, FSharpPlus.Math.Generic.(/) one)