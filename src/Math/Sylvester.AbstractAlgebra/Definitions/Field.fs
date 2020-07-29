﻿namespace Sylvester

open System.Collections

open Sylvester.Arithmetic
open Sylvester.Collections

/// Set of elements closed under a left-associative commutative invertible operation and a 2nd left-associative commutative invertible operation whcih distributes over the first.
type IField<'t when 't: equality> =  
    abstract AddGroup:IAdditiveGroup<'t>
    abstract MulGroup:IMultiplicativeGroup<'t>
    
/// Set of elements closed under a left-associative commutative invertible operation and a 2nd left-associative commutative invertible operation whcih distributes over the first.
type Field<'t when 't: equality>(additiveGroup: IAdditiveGroup<'t>, multiplicativeGroup: IMultiplicativeGroup<'t>) =
    inherit Struct<'t, card.six>(additiveGroup.Set, arrayOf6 (Binary(additiveGroup.Op)) (Nullary(additiveGroup.Identity)) (Unary(additiveGroup.Inverse)) (Binary(additiveGroup.Op)) (Nullary(additiveGroup.Identity)) (Unary(additiveGroup.Inverse)))
    do multiplicativeGroup.Op |> failIfNotDistributiveOver additiveGroup.Op
    member val AddGroup = additiveGroup
    member val MulGroup = multiplicativeGroup
    interface IField<'t> with
        member val AddGroup = additiveGroup
        member val MulGroup = multiplicativeGroup

[<AutoOpen>]
module Real =
    let reals = let x = var<Real> in SetComprehension<Real>(x, (fun _ _ -> true)) |> Set 
    let R = Field(AdditiveGroup(reals), MultiplicativeGroup(reals))
    let openInterval left right = R.Set.Subset(fun x -> x > left && x < right)
    let closedinterval left right = R.Set.Subset(fun x -> x >= left && x <= right)
    let line (origin:R) (step:R) = infiniteSeq (fun n -> origin + (((float) n) * step))
    let axis step = line 0.0 step