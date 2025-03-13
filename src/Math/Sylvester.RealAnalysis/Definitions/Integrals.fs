namespace Sylvester

module Integrals =
    let lower_riemann_sum (a:real) (b:real) (p:seq<real>) (f:RealFunction) =
        let min,max = Seq.min p, Seq.max p
        if min <> a || max <> b || (p |> Seq.pairwise |> Seq.forall(fun(l,r) -> r > l) |> not) then failwithf "The partition does not cover the interval [%A,%A]." a b
        p |> Seq.pairwise |> Seq.sumBy(fun (l,r) -> (seq {l .. 0.01 .. r} |> Seq.map f.Map |> Seq.min) * (r - l))

    let upper_riemann_sum (a:real) (b:real) (p:seq<real>) (f:RealFunction) =
        let min,max = Seq.min p, Seq.max p
        if min <> a || max <> b || (p |> Seq.pairwise |> Seq.forall(fun(l,r) -> r > l) |> not) then failwithf "The partition does not cover the interval [%A,%A]." a b
        p |> Seq.pairwise |> Seq.sumBy(fun (l,r) -> (seq {l .. 0.01 .. r} |> Seq.map f.Map |> Seq.max) * (r - l))

    let lower_uniform_riemann_sum (a:real) (b:real) (n:int) (f:RealFunction) =
        let dx = (b - a) / real n in
        lower_riemann_sum a b (seq {a.. dx ..b}) f

    let upper_uniform_riemann_sum (a:real) (b:real) (n:int) (f:RealFunction) =
        let dx = (b - a) / real n in
        upper_riemann_sum a b (seq {a.. dx ..b}) f
    
    (*
    let integrate_changevar (x:ScalarVar<real>) (eqn:ScalarEquation<real>) (s:ISymbolic<_, real>) =
        let m = as_var_map eqn
        let v = m.Var
        let vars =  get_vars m.Rhs
        if v = x || vars.Length <> 1 || not (has_var x.Var vars) then failwithf "%A is not a scalar function of %A" eqn x
        let dx = 1 / diff x (m.Rhs) 
        let sv = subst_var_value x.Var dx.Expr s.Expr |> expand_as<real> |> s.Transform
        sv
        *)
