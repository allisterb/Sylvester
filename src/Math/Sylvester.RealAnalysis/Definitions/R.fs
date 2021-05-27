namespace Sylvester

open FSharp.Quotations

open Arithmetic
open Scalar
open Vector

type R<'n when 'n :>Number>() = 
    inherit VectorSpace<'n, real, Vector<'n, real>>(Field.R, Vector.add, Vector.smul)
    
[<AutoOpen>]
module R =
    let R<'n when 'n :> Number> =  R<'n>()
    
    let internal Ops = RealAnalysis.defaultRealAnalysisSymbolicOps

    let realvar n = var'<real> n

    let real_expr x = 
        match box x with
        | :? Scalar<real> as s -> s.Expr
        | :? Expr<real> as e -> e
        | :? real as n -> Expr.Value n |> expand''<real>
        | :? int as n -> Expr.Value (real n) |> expand''<real>
        | _ -> failwithf "The expression %A is not a real number expression." x

    let open_interval left right = Field.R |>| (fun x -> x > left && x < right)
    
    let closed_interval left right = Field.R |>| (fun x -> x >= left && x <= right)

    let half_open_interval left right = Field.R |>| (fun x -> x > left && x <= right)
    
    let half_closed_interval left right = Field.R |>| (fun x -> x >= left && x < right)
    
    let open_ball (x:Vec<_>) (r:real) = R |>| (fun y -> (euclid_dist x y) < scalar r)
    
    let lim f x v = Ops.Limit f x v |> Scalar
       
    let lim_right f x v = Ops.LimitRight f x v |> Scalar

    let lim_left f x v = Ops.LimitLeft f x v |> Scalar

    let inline deriv_lim f x a = 
        lim <@ ((%f)(%x + %a) - (%f) %x) / %a @> a <@ 0. @>

    let diff f  =  Ops.Diff f 1

    let integrate f = Ops.Integrate f

    let definite_integral f (l:'a) (r:'b) = Ops.DefiniteIntegral f (real_expr l) (real_expr r) |> Scalar<real>