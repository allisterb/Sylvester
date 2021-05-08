namespace Sylvester

open FSharp.Quotations

open Sylvester.Arithmetic
open Vector

type R<'n when 'n :>Number>() = 
    inherit VectorSpace<'n, real, Vector<'n, real>>(Field.R, Vector.add, Vector.smul)
    
[<AutoOpen>]
module R =
    let R<'n when 'n :> Number> =  R<'n>()
    
    let open_interval left right = Field.R |>| (fun x -> x > left && x < right)
    
    let closed_interval left right = Field.R |>| (fun x -> x >= left && x <= right)
    
    let open_ball (x:Vec<_>) (r:real) = R |>| (fun y -> (euclid_dist x y) < Scalar r)
    
    let lim f x v : Scalar<real> = defaultRealAnalysisSymbolicOps.Limit f x v |> Scalar
       
    let lim_right f x v = defaultRealAnalysisSymbolicOps.LimitRight f x v |> Scalar

    let lim_left f x v = defaultRealAnalysisSymbolicOps.LimitLeft f x v |> Scalar

    let diff f x  =  defaultRealAnalysisSymbolicOps.Diff f x 1 |> Scalar

    let inline deriv f x a = 
        lim <@ ((%f)(%x + %a) - (%f) %x) / %a @> a <@ 0. @>

