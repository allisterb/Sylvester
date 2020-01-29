namespace Sylvester

open Sylvester.Arithmetic
open Sylvester.Collections

/// Set of elements closed under some left-associative operation with identity and an inverse unary operation.
type IGroup<'t when 't: equality> = 
    inherit IMonoid<'t> 
    abstract member Inverse: UnaryOp<'t>

/// Set of elements closed under some left-associative operation with identity and an inverse unary operation.
type Group<'t when 't: equality>(set:Set<'t>, op:BinaryOp<'t>, id:'t, inv: UnaryOp<'t>) =
    inherit Monoid<'t>(set, op, id)
    member val Inverse = inv
    interface IGroup<'t> with member val Inverse = inv

type AbelianGroup<'t when 't: equality>(set:Set<'t>, op: BinaryOp<'t>, id:'t, inv: UnaryOp<'t>) =
    inherit Group<'t>(set, op, id, inv)
    do failIfNotCommutative op

/// Category of groups with n structure-preserving morphisms.
type Groups<'ut, 'vt, 'n when 'ut : equality and 'vt: equality and 'n :> Number>(l:Group<'ut>, r:Group<'vt>, maps: Array<'n, Map<'ut, 'vt>>) = 
    inherit Category<'ut, 'vt, card.one, card.one, 'n>(l, r, maps) 

[<AutoOpen>]
module Group =
    /// Define a group over a set which has an additive operator and zero and negation. 
    let inline AdditiveGroup<'t when 't : equality and 't : (static member Zero:'t) and 't: (static member (+) :'t -> 't -> 't) and 't: (static member (~-) :'t -> 't)> 
        (set: Set<'t>) =
        let id = LanguagePrimitives.GenericZero<'t>
        AbelianGroup(set, Binary(+).DestructureBinary, id, (~-))

    /// Define a group over a set which has a multiplicative operator and one and division.
    let inline MultiplicativeGroup<'t when 't : equality and 't : (static member One:'t) and 't: (static member (*) :'t -> 't -> 't) and 't: (static member (/) :'t -> 't -> 't)>
        (set: Set<'t>) =
        let one = LanguagePrimitives.GenericOne<'t>
        AbelianGroup(set, FSharpPlus.Math.Generic.(*), one, FSharpPlus.Math.Generic.(/) one)