namespace Sylvester

open Sylvester.Collections

/// Set of elements closed under a left-associative commutative invertible operation and a 2nd left-associative commutative invertible operation whcih distributes over the first.
type IField<'t when 't: equality> =  
    abstract AddGroup:IAdditiveGroup<'t>
    abstract MulGroup:IMultiplicativeGroup<'t>
    
/// Set of elements closed under a left-associative commutative invertible operation and a 2nd left-associative commutative invertible operation whcih distributes over the first.
type Field<'t when 't: equality>(additiveGroup: IAdditiveGroup<'t>, multiplicativeGroup: IMultiplicativeGroup<'t>) =
    inherit Struct<'t, card.six>(additiveGroup.Set, arrayOf6 (Binary(additiveGroup.Op)) (Nullary(additiveGroup.Identity)) (Unary(additiveGroup.Inverse)) (Binary(additiveGroup.Op)) (Nullary(additiveGroup.Identity)) (Unary(additiveGroup.Inverse)))
    do multiplicativeGroup.Op |> fail_if_not_distributive_over additiveGroup.Op
    member val AddGroup = additiveGroup
    member val MulGroup = multiplicativeGroup
    interface IField<'t> with
        member val AddGroup = additiveGroup
        member val MulGroup = multiplicativeGroup

[<AutoOpen>]
module Field = 
    let R = 
        let reals = let x = var<real> in SetComprehension<real>(<@ x @>, (Aleph 1), (fun _ _ -> true)) |> Set in
        Field(AdditiveGroup(reals), MultiplicativeGroup(reals))
    let open_interval left right = R.Set.Subset(fun x -> x > left && x < right)
    let closed_interval left right = R.Set.Subset(fun x -> x >= left && x <= right)
    let line (origin:real) (step:real) = infinite_seq (fun n -> origin + ((float n) * step))
    let axis step = line 0.0 step