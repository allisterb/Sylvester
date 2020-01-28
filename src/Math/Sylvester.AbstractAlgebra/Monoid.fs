namespace Sylvester

type IMonoid<'U when 'U: equality> = 
    inherit IGroupoid<'U>
    abstract member Identity: 'U

/// Set of elements closed under some left-associative operation with identity.
type Monoid<'U when 'U: equality>(set:Set<'U>, op:BinaryOp<'U>, id:'U) =
    inherit Semigroup<'U>(set, op)
    member val Op = op
    member val Identity = id
    interface IMonoid<'U> with member val Identity = id
    
type CommutativeMonoid<'U when 'U: equality>(set:Set<'U>, op:BinaryOp<'U>, id:'U) =
    inherit Monoid<'U>(set, op, id)
    do failIfNotCommutative op

/// Category of monoids with a structure-preserving morphism.
type Mon<'U when 'U : equality> = Category<'U, Monoid<'U>, card.one>

[<AutoOpen>]
module Monoid =
    /// Define a monoid over a set which has an additive operator and zero. 
    let inline AdditiveMonoid<'U when 'U : equality and 'U : (static member Zero:'U) and 'U: (static member (+) :'U -> 'U -> 'U)> 
        (set: Set<'U>) =
        let id = LanguagePrimitives.GenericZero<'U>
        CommutativeMonoid(set, Binary(+).DestructureBinary, id)

    /// Define a monoid over a set which has an multiplicative operator and one. 
    let inline MultiplicativeMonoid<'U when 'U : equality and 'U : (static member One:'U) and 'U: (static member (*) :'U -> 'U -> 'U)> 
        (set: Set<'U>) =
        let one = LanguagePrimitives.GenericOne<'U>
        CommutativeMonoid(set, FSharpPlus.Math.Generic.(*), one)



