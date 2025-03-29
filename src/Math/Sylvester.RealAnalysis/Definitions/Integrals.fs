namespace Sylvester

module Integrals =
    let lower_riemann_sum (l:obj) (r:obj) (part:obj seq) (f:RealFunction) =
        let a,b,p = to_real l, to_real r, part |> Seq.map to_real
        if Seq.min p <> a || Seq.max p <> b || (p |> Seq.pairwise |> Seq.forall(fun(l,r) -> r > l) |> not) then failwithf "The partition does not cover the interval [%A,%A]." a b
        p |> Seq.pairwise |> Seq.sumBy(fun (l,r) -> (seq {l.. 0.01 .. r} |> Seq.map f.Item |> Seq.min) * (r - l))

    let upper_riemann_sum (l:obj) (r:obj) (part:obj seq) (f:RealFunction) =
        let a,b,p = to_real l, to_real r, part |> Seq.map to_real
        if Seq.min p <> a || Seq.max p <> b || (p |> Seq.pairwise |> Seq.forall(fun(l,r) -> r > l) |> not) then failwithf "The partition does not cover the interval [%A,%A]." a b
        p |> Seq.pairwise |> Seq.sumBy(fun (l,r) -> (seq {l .. 0.01 .. r} |> Seq.map f.Item |> Seq.max) * (r - l))

    let lower_uniform_riemann_sum (l:obj) (r:obj) (n:int) (f:RealFunction) =
        let a,b = to_real l, to_real r
        let dx = (b - a) / real n in
        lower_riemann_sum a b (seq {a .. dx .. b} |> Seq.map box) f

    let upper_uniform_riemann_sum (l:obj) (r:obj) (n:int) (f:RealFunction) =
        let a,b = to_real l, to_real r
        let dx = (b - a) / real n in
        upper_riemann_sum a b (seq {a .. dx .. b} |> Seq.map box) f
    
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
