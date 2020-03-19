namespace Sylvester

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
module Field =
    let float32s = SetBuilder(fun (_:float32) -> true) |> Set
    let floats = SetBuilder(fun (_:float) -> true) |> Set
    
    let ReF = Field(AdditiveGroup(float32s), MultiplicativeGroup(float32s))
    let Re = Field(AdditiveGroup(floats), MultiplicativeGroup(floats))

    let openInterval left right = Re.Set.Subset(fun x -> x > left && x < right)
        
    let closedinterval left right = Re.Set.Subset(fun x -> x >= left && x <= right)
    
    let lineF (origin:float32) (step:float32) = infiniteSeq (fun x -> x >= origin) (fun n -> origin + (((float32) n) * step))
    let line (origin:float) (step:float) = infiniteSeq (fun x -> x >= origin) (fun n -> origin + (((float) n) * step))
    
    let axisF step = lineF 0.0F step
    let axis step = line 0.0 step