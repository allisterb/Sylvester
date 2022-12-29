namespace Sylvester

open Sylvester
open Arithmetic
open N10
open Sylvester.Collections
/// Set of elements closed under a left-associative commutative invertible operation and a 2nd left-associative commutative invertible operation whcih distributes over the first.
type IField<'t when 't: equality> =  
    inherit ISet<'t>
    abstract AddGroup:IAdditiveGroup<'t>
    abstract MulGroup:IMultiplicativeGroup<'t>
    
/// Set of elements closed under a left-associative commutative invertible operation and a 2nd left-associative commutative invertible operation whcih distributes over the first.
type Field<'t when 't: equality>(additiveGroup: IAdditiveGroup<'t>, multiplicativeGroup: IMultiplicativeGroup<'t>) =
    inherit Struct<'t,  ``6``>(additiveGroup.Set, arrayOf6 (Binary(additiveGroup.Op)) (Nullary(additiveGroup.Identity)) (Unary(additiveGroup.Inverse)) (Binary(additiveGroup.Op)) (Nullary(additiveGroup.Identity)) (Unary(additiveGroup.Inverse)))
    do  fail_if_not_distributive_over multiplicativeGroup.Op additiveGroup.Op
    member val AddGroup = additiveGroup
    member val MulGroup = multiplicativeGroup
    interface IField<'t> with
        member val Set = additiveGroup.Set
        member val AddGroup = additiveGroup
        member val MulGroup = multiplicativeGroup

type OrderedField<'t when 't: equality and 't : comparison>(additiveGroup: IAdditiveGroup<'t>, multiplicativeGroup: IMultiplicativeGroup<'t>) =
    inherit Field<'t>(additiveGroup, multiplicativeGroup)
    interface ISet<'t> with
        member val Set = additiveGroup.Set
        member x.Equals y = x.Set.Equals y 
    interface ITotalOrder<'t> with
        member val Domain = additiveGroup.Set
        member val CoDomain = additiveGroup.Set
        member val Op = <@ (<) @>

module Field = 
    let R = 
        let reals = let x = var'<real> "x" in SetComprehension(x, Aleph 1) |> Set
        OrderedField(additive_group(reals), multiplicative_group(reals))