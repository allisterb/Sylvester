namespace Sylvester

open FSharp.Quotations

open Arithmetic
open Vector

type Region<'n when 'n :> Number> = Set<Vec<'n>>

type R<'n when 'n :>Number>() = 
    inherit VectorSpace<'n, real, Vector<'n, real>>(Field.R, Vector.add, Vector.smul)
    
[<AutoOpen>]
module R =
    let R<'n when 'n :> Number> =  R<'n>()
    
    let sum expr x l u = Ops.Sum expr x (int_expr l) (int_expr u) |> Term

    let open_interval left right = Field.R |>| <@ fun x -> x > left && x < right @>
    
    let closed_interval left right = Field.R |>| <@ fun x -> x >= left && x <= right @>

    let half_open_interval left right = Field.R |>| <@ fun x -> x > left && x <= right @>
    
    let half_closed_interval left right = Field.R |>| <@ fun x -> x >= left && x < right @>
    
    //let open_ball (x:Vec<_>) (r:real) : Region<_> = Field.R |>| <@ fun y -> (euclid_dist x y) < r @>
    
    let lim f x v = Ops.Limit f x v |> Term
       
    let lim_right f x v = Ops.LimitRight f x v |> Term

    let lim_left f x v = Ops.LimitLeft f x v |> Term

    let inline deriv_lim f x a = 
        lim <@ ((%f)(%x + %a) - (%f) %x) / %a @> a <@ 0. @>

    let diff (x:Term<real>) (f:Term<real>) =  Ops.Diff 1 x.Expr f.Expr |> Term

    let integrate f x = Ops.Integrate f x

    let integrate_over l r f = Ops.DefiniteIntegral f (real_expr l) (real_expr r) |> Term

    let integrate_over_R f = Ops.DefiniteIntegral f minf'<real> inf'<real> |> Term