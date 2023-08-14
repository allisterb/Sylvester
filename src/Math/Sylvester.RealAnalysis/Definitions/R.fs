namespace Sylvester

open Arithmetic
open Vector

type Region<'n when 'n :> Number> = Set<Vec<'n>>

type R<'n when 'n :>Number>() = 
    inherit VectorSpace<'n, real>(Field.R, Vector.add, Vector.smul)
    interface ICardinality with member val Cardinality = Aleph 1
[<AutoOpen>]
module R =
    //let R (dim:'n when 'n :> Number) =  R<'n>()
    
    let sum x l u expr = Ops.Sum x (intexpr l) (intexpr u) expr |> Scalar

    let open_interval left right = Field.R |>| <@ fun x -> x > left && x < right @>
    
    let closed_interval left right = Field.R |>| <@ fun x -> x >= left && x <= right @>

    let half_open_interval left right = Field.R |>| <@ fun x -> x > left && x <= right @>
    
    let half_closed_interval left right = Field.R |>| <@ fun x -> x >= left && x < right @>
    
    //let open_ball (x:Vec<_>) (r:real) : Region<_> = Field.R |>| <@ fun y -> (euclid_dist x y) < scalar r @>
    
    let lim x v f = Ops.Limit x v f |> Scalar
       
    let lim_right x v f = Ops.LimitRight x v f |> Scalar

    let lim_left x v f = Ops.LimitLeft x v f |> Scalar

    let inline deriv_lim f x a = 
        lim <@ ((%f)(%x + %a) - (%f) %x) / %a @> a <@ 0. @>

    let diff (x:Scalar<real>) (s:ISymbolic<_, real>) =  
        fail_if_not_var x
        fail_if_not_has_var (get_var x.Expr) s.Expr
        s.Mutate(Ops.Diff 1 x.Expr s.Expr)

    let integrate (x:Scalar<real>) (s:ISymbolic<_, real>) = 
        fail_if_not_var x
        fail_if_not_has_var (get_var x.Expr) s.Expr
        s.Mutate(Ops.Integrate x.Expr s.Expr)

    let integrate_over (x:Scalar<real>) l r (s:ISymbolic<_, real>) = 
        fail_if_not_var x
        fail_if_not_has_var (get_var x.Expr) s.Expr
        s.Mutate(Ops.DefiniteIntegral x.Expr (realexpr l) (realexpr r) s.Expr)

    let integrate_over_R (x:Scalar<real>) f = integrate_over x minf'<real> inf'<real> f