﻿namespace Sylvester

open System
open Z3

type Region = Set<Vec>

type RealVectorSpace(n:int) = 
    inherit VectorSpace<real>(n, Field.R, Vector.vadd, Vector.vsmul)
    interface ICardinality with member val Cardinality = Aleph 1

[<RequireQualifiedAccess>]
module Reals=
    let e = Math.e |> Scalar
    let pi = Math.pi |> Scalar

[<AutoOpen>]
module R =
    let R (n:int) = new RealVectorSpace(n)
             
    let algexpand (x:ISymbolic<_, real>) = x.Transform(x |> sexpr |> Ops.AlgExpand)

    let ratexpand (x:ISymbolic<_, real>) = x.Transform(x |> sexpr |> Ops.RatExpand)
    
    let ratsimp (x:ISymbolic<_, real>) = x.Transform(x |> sexpr |> Ops.RatSimp)

    let factor (x:ISymbolic<_, real>) = x.Transform(x |> sexpr |> Ops.Factor)

    let factor_for (p:Scalar<real>) (x:ISymbolic<_, real>) = x.Transform(x |> sexpr |> Ops.FactorFor p.Expr)

    let partfrac_of (f:Scalar<real>) (x:ISymbolic<_, real>) = x.Transform(x |> sexpr |> Ops.PartFracOf f.Expr)

    let trigsimp (x:ISymbolic<_, real>) = x.Transform(x |> sexpr |> Ops.TrigSimp)

    let trigexpand (x:ISymbolic<_, real>) = x.Transform(x |> sexpr |> Ops.TrigExpand)
       
    let trigreduce (x:ISymbolic<_, real>) = x.Transform(x |> sexpr |> Ops.TrigReduce)

    let simplify (x:ISymbolic<_, real>) = x.Transform(x |> simplify, null, ?s=x.Symbol)
       
    let maximize  (x:ISymbolic<_, real>) (c:ScalarRelation<real> seq)  = 
        let s = new Z3Solver()
  
        c |> Seq.toList |> List.map sexpr |> opt_assert_hard s
        let _ = opt_maximize s (x.Expr)
        if opt_check_sat s then 
            let sols = opt_get_rat_var_model s
            (s :> IDisposable).Dispose()
            sols |> Option.map(fun s -> s |> List.map (fun sol -> let v = realvar (fst sol) in ScalarVarMap(v, (sol |> snd |> real |> exprv |> Scalar<real>))))
        else 
            (s :> IDisposable).Dispose()
            None
        
    let minimize (x:ISymbolic<_, real>) (c:ScalarRelation<real> seq) = 
        let s = new Z3Solver()
        c |> Seq.toList |> List.map sexpr |> opt_assert_hard s
        let _ = opt_minimize s (x.Expr)
        if opt_check_sat s then 
            let sols = opt_get_rat_var_model s
            (s :> IDisposable).Dispose()
            sols |> Option.map(fun s -> s |> List.map (fun sol -> let v = realvar (fst sol) in ScalarVarMap(v, (sol |> snd |> real |> exprv |> Scalar<real>))))
        else 
            (s :> IDisposable).Dispose()
            None

    let sum x l u expr = Ops.Sum x (intexpr l) (intexpr u) expr |> Scalar

    let open_interval left right = Field.R |>| <@ fun x -> x > left && x < right @>
    
    let closed_interval left right = Field.R |>| <@ fun x -> x >= left && x <= right @>

    let half_open_interval left right = Field.R |>| <@ fun x -> x > left && x <= right @>
    
    let half_closed_interval left right = Field.R |>| <@ fun x -> x >= left && x < right @>
    
    //let open_ball (x:Vec<_>) (r:real) : Region<_> = Field.R |>| <@ fun y -> (euclid_dist x y) < scalar r @>
    
    let lim (x:ScalarVar<real>) (v:Scalar<real>) (f:ISymbolic<_, real>) = fail_if_not_has_var x.Var f.Expr; Ops.Limit (x.Expr) (v.Expr) f.Expr |> Scalar
       
    let lim_right (x:ScalarVar<real>) (v:Scalar<real>) (f:ISymbolic<_, real>) = fail_if_not_has_var x.Var f.Expr; Ops.LimitRight (x.Expr) (v.Expr) f.Expr |> Scalar

    let lim_left (x:ScalarVar<real>) (v:Scalar<real>) (f:ISymbolic<_, real>) = fail_if_not_has_var x.Var f.Expr; Ops.LimitLeft (x.Expr) (v.Expr) f.Expr |> Scalar

    let inline deriv_lim f x a = 
        Ops.Limit <@ ((%f)(%x + %a) - (%f) %x) / %a @> a <@ 0. @>

    let diff (x:ScalarVar<real>) (s:ISymbolic<'a, real>) = 
        do fail_if_not_has_var x.Var s.Expr
        match s.Symbol with
        | None -> s.Transform(Ops.Diff 1 x.Expr s.Expr, newattrs [("Derivative", box true)])
        | Some sym ->  
            let n = if s.Expr |> get_vars |> List.length > 1 then sym.JoinSuperscript(x.Name) else sym + "'"
            s.Transform(Ops.Diff 1 x.Expr s.Expr, newattrs [("Derivative", box true)], n)

    let diffe (x:ScalarVar<real>) (s:ISymbolic<'a, real>) =
        do fail_if_not_has_var x.Var s.Expr
        Ops.Diff 1 x.Expr s.Expr |> Scalar<real> |> with_attr_tag "Derivative"

    let integrate (x:ScalarVar<real>) (s:ISymbolic<_, real>) = 
        match s.Symbol with 
        | None -> s.Transform(Ops.Integrate x.Expr s.Expr, newattrs [("Integral", box true)])
        | Some sym -> 
            let n = if sym.Length = 1 && System.Char.IsLower(sym.[0]) then sym.Replace(sym.[0], System.Char.ToUpper(sym.[0])) else "I" + sym.JoinSuperscript(x.Name)
            s.Transform(Ops.Integrate x.Expr s.Expr, newattrs [("Integral", box true)], n)
        
    let integrate_over (x:ScalarVar<real>) l r (s:ISymbolic<_, real>) = Ops.DefiniteIntegral x.Expr (realexpr l) (realexpr r) s.Expr |> Scalar

    let integrate_over_R (x:ScalarVar<real>) f = integrate_over x neginf inf f

    let taylor_series (x:ScalarVar<real>) (a:obj) (n:int) (s:ISymbolic<_, real>) =
        CAS.Analysis.taylor_series s.Expr x.Expr (a |> sterm<real> |> sexpr) n |> Scalar<real>


