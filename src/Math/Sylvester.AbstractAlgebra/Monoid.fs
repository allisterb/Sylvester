namespace Sylvester

open Sylvester.Collections

/// Set of elements closed under some left-associative operation with identity.
type IMonoid<'t when 't: equality> = 
    inherit IGroupoid<'t>
    abstract member Identity: 't

/// Set of elements closed under some left-associative operation with identity.
type Monoid<'t when 't: equality>(set:Set<'t>, op:BinaryOp<'t>, id:'t) =
    inherit Semigroup<'t>(set, op)
    member val Op = op
    member val Identity = id
    interface IMonoid<'t> with member val Identity = id
    
/// Monoid with commutative operators.
type CommutativeMonoid<'t when 't: equality>(set:Set<'t>, op:BinaryOp<'t>, id:'t) =
    inherit Monoid<'t>(set, op, id)
    do failIfNotCommutative op

/// Category of monoids with a structure-preserving morphism.
type Monoids<'ut, 'vt when 'ut : equality and 'vt: equality>(l:Monoid<'ut>, r:Monoid<'vt>, map: Map<'ut, 'vt>) = 
    inherit Category<'ut, 'vt, card.one, card.one, Monoid<'ut>, Monoid<'vt>, card.one>(Morph(l, r, map) |> arrayOf1)

[<AutoOpen>]
module Monoid =
    /// Define a monoid over a set which has an additive operator and zero. 
    let inline AdditiveMonoid<'t when 't : equality and 't : (static member Zero:'t) and 't: (static member (+) :'t -> 't -> 't)> 
        (set: Set<'t>) =
        let id = LanguagePrimitives.GenericZero<'t>
        CommutativeMonoid(set, Binary(+).DestructureBinary, id)

    /// Define a monoid over a set which has an multiplicative operator and one. 
    let inline MultiplicativeMonoid<'t when 't : equality and 't : (static member One:'t) and 't: (static member (*) :'t -> 't -> 't)> 
        (set: Set<'t>) =
        let one = LanguagePrimitives.GenericOne<'t>
        CommutativeMonoid(set, FSharpPlus.Math.Generic.(*), one)