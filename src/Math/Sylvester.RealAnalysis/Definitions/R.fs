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

    let diff (x:Term<real>) (s:ISymbolic<_, real>) =  
        fail_if_not_var x
        fail_if_not_has_var (get_var x.Expr) s.Expr
        s.Mutate(Ops.Diff 1 x.Expr s.Expr)

    let integrate (x:Term<real>) (f:Term<real>)  = Ops.Integrate x.Expr f.Expr |> Term

    let integrate_over (x:Term<real>) l r (f:Term<real>) = Ops.DefiniteIntegral x.Expr (real_expr l) (real_expr r) f.Expr |> Term

    let integrate_over_R (x:Term<real>) f = integrate_over x minf'<real> inf'<real> f