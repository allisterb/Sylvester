namespace Sylvester

open FSharp.Quotations

open Sylvester.Arithmetic
open Vector

type R<'n when 'n :>Number>() = 
    inherit VectorSpace<'n, real, Vector<'n, real>>(Field.R, Vector.vadd, Vector.vsmul)
    
[<AutoOpen>]
module R =
    let R<'n when 'n :> Number> =  R<'n>()
    
    [<Formula>]
    let open_interval left right = Field.R |>| (fun x -> x > left && x < right)
    
    [<Formula>]
    let closed_interval left right = Field.R |>| (fun x -> x >= left && x <= right)
    
    [<Formula>]
    let open_ball (x:Vec<_>) (r:real) = R |>| (fun y -> (euclid_dist x y) < Scalar r)

    let part_deriv (f:Expr<Vec<_>->real>) = ()

    let lim f x v = defaultRealAnalysisSymbolicOps.Limit f x v

    let lim_right f x v = defaultRealAnalysisSymbolicOps.LimitRight f x v

    let lim_left f x v = defaultRealAnalysisSymbolicOps.LimitLeft f x v

    let diff f x  =  defaultRealAnalysisSymbolicOps.Diff f x 1

    let inline deriv f x a = 
        lim <@ ((%f)(%x + %a) - (%f) %x) / %a @> a <@ 0. @>
