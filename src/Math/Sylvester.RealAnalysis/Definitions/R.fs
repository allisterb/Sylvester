namespace Sylvester

open Arithmetic
open Z3
open Vector

type Region<'n when 'n :> Number> = Set<Vec<'n>>

type RealVectorSpace<'n when 'n :>Number>() = 
    inherit VectorSpace<'n, real>(Field.R, Vector.add, Vector.smul)
    interface ICardinality with member val Cardinality = Aleph 1

[<AutoOpen>]
module R =
    let R (dim:'n when 'n :> Number) = new RealVectorSpace<'n>()
             
    let algexpand (x:ISymbolic<_, real>) = x.Mutate(x |> sexpr |> Ops.AlgExpand)

    let ratexpand (x:ISymbolic<_, real>) = x.Mutate(x |> sexpr |> Ops.RatExpand)
    
    let ratsimp (x:ISymbolic<_, real>) = x.Mutate(x |> sexpr |> Ops.RatSimp)

    let trigsimp (x:ISymbolic<_, real>) = x.Mutate(x |> sexpr |> Ops.TrigSimp)

    let trigexpand (x:ISymbolic<_, real>) = x.Mutate(x |> sexpr |> Ops.TrigExpand)
       
    let trigreduce (x:ISymbolic<_, real>) = x.Mutate(x |> sexpr |> Ops.TrigReduce)

    let simplify (x:ISymbolic<_, real>) = x.Mutate(x |> simplify)

    let maximize (s:Z3Solver) (c:ScalarRelation<real> list) (x:ISymbolic<_, real>)  = 
        c |> List.map sexpr |> opt_assert_hard s
        let _ = opt_maximize s (x.Expr)
        if opt_check_sat s then 
            let sols = opt_get_rat_var_model s
            sols |> Option.map(fun s -> s |> List.map (fun sol -> let v = ratvar (fst sol) in ScalarVarMap(v, (sol |> snd |> exprv |> Scalar<rat>))))
        else None


    let sum x l u expr = Ops.Sum x (intexpr l) (intexpr u) expr |> Scalar

    let open_interval left right = Field.R |>| <@ fun x -> x > left && x < right @>
    
    let closed_interval left right = Field.R |>| <@ fun x -> x >= left && x <= right @>

    let half_open_interval left right = Field.R |>| <@ fun x -> x > left && x <= right @>
    
    let half_closed_interval left right = Field.R |>| <@ fun x -> x >= left && x < right @>
    
    //let open_ball (x:Vec<_>) (r:real) : Region<_> = Field.R |>| <@ fun y -> (euclid_dist x y) < scalar r @>
    
    let lim (x:ScalarVar<real>) (v:Scalar<real>) (f:ISymbolic<_, real>) = fail_if_not_has_var x.Var f.Expr; Ops.Limit (x.Expr) (v.Expr) f.Expr |> f.Mutate
       
    let lim_right (x:ScalarVar<real>) (v:Scalar<real>) (f:ISymbolic<_, real>) = fail_if_not_has_var x.Var f.Expr; Ops.LimitRight (x.Expr) (v.Expr) f.Expr |> f.Mutate

    let lim_left (x:ScalarVar<real>) (v:Scalar<real>) (f:ISymbolic<_, real>) = fail_if_not_has_var x.Var f.Expr; Ops.LimitLeft (x.Expr) (v.Expr) f.Expr |> f.Mutate

    let inline deriv_lim f x a = 
        Ops.Limit <@ ((%f)(%x + %a) - (%f) %x) / %a @> a <@ 0. @>

    let diff (x:ScalarVar<real>) (s:ISymbolic<_, real>) = fail_if_not_has_var x.Var s.Expr; s.Mutate(Ops.Diff 1 x.Expr s.Expr)

    let integrate (x:ScalarVar<real>) (s:ISymbolic<_, real>) = fail_if_not_has_var x.Var s.Expr; s.Mutate(Ops.Integrate x.Expr s.Expr)

    let integrate_over (x:ScalarVar<real>) l r (s:ISymbolic<_, real>) = fail_if_not_has_var x.Var s.Expr; s.Mutate(Ops.DefiniteIntegral x.Expr (realexpr l) (realexpr r) s.Expr)

    let integrate_over_R (x:ScalarVar<real>) f = integrate_over x minf'<real> inf'<real> f


