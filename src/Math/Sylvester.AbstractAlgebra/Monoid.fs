namespace Sylvester

open Sylvester.Arithmetic
open Sylvester.Collections

/// Set of elements closed under some left-associative operation with identity.
type IMonoid<'t when 't: equality> = 
    inherit IGroupoid<'t>
    abstract member Identity: 't

/// Set of elements closed under some left-associative operation with identity element.
type Monoid<'t when 't: equality>(set:ISet<'t>, op:BinaryOp<'t>, id: NullaryOp<'t>) =
    inherit Semigroup<'t>(set, op)
    member val Op = op
    member val Identity = id
    interface IMonoid<'t> with member val Identity = id
    
/// Monoid with commutative operators.
type CommutativeMonoid<'t when 't: equality>(set:ISet<'t>, op:BinaryOp<'t>, id:'t) =
    inherit Monoid<'t>(set, op, id)
    do failIfNotCommutative op

/// Category of monoids with n structure-preserving morphisms.
type Monoids<'ut, 'vt, 'n when 'ut : equality and 'vt: equality and 'n :> Number>(l:Monoid<'ut>, r:Monoid<'vt>, maps: Array<'n, Map<'ut, 'vt>>) = 
    inherit Category<'ut, 'vt, card.one, card.one,'n>(l, r, maps)

[<AutoOpen>]
module Monoid =
    /// Define a monoid over a set which has an additive operator and zero. 
    let inline AdditiveMonoid<'t when 't : equality and 't : (static member Zero:'t) and 't: (static member (+) :'t -> 't -> 't)> 
        (set: ISet<'t>) =
        let id = LanguagePrimitives.GenericZero<'t>
        CommutativeMonoid(set, Binary(+).DestructureBinary, id)

    /// Define a monoid over a set which has a multiplicative operator and one. 
    let inline MultiplicativeMonoid<'t when 't : equality and 't : (static member One:'t) and 't: (static member (*) :'t -> 't -> 't)> 
        (set: ISet<'t>) =
        let one = LanguagePrimitives.GenericOne<'t>
        CommutativeMonoid(set, FSharpPlus.Math.Generic.(*), one)