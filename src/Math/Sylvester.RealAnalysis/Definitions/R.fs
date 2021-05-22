namespace Sylvester

open FSharp.Quotations

open Sylvester.Arithmetic
open Vector

type R<'n when 'n :>Number>() = 
    inherit VectorSpace<'n, real, Vector<'n, real>>(Field.R, Vector.add, Vector.smul)
    
[<AutoOpen>]
module R =
    let R<'n when 'n :> Number> =  R<'n>()
    
    let internal Ops = RealAnalysis.defaultRealAnalysisSymbolicOps

    let open_interval left right = Field.R |>| (fun x -> x > left && x < right)
    
    let closed_interval left right = Field.R |>| (fun x -> x >= left && x <= right)
    
    let open_ball (x:Vec<_>) (r:real) = R |>| (fun y -> (euclid_dist x y) < Scalar r)
    
    let lim f x v : Scalar<real> = Ops.Limit f x v |> Scalar
       
    let lim_right f x v = Ops.LimitRight f x v |> Scalar

    let lim_left f x v = Ops.LimitLeft f x v |> Scalar

    let diff f x  =  Ops.Diff f x 1

    let integrate f x = Ops.Integrate f x 

    let inline deriv f x a = 
        lim <@ ((%f)(%x + %a) - (%f) %x) / %a @> a <@ 0. @>

