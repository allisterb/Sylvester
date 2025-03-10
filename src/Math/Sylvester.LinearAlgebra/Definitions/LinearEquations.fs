namespace Sylvester

open Sylvester
open Sylvester.CAS

module LinearEquations =
    
    let solve (o:'a) (v:seq<ScalarVar<'t>>)  (eqns: ScalarEquation<'t> list) =
        let _eqns = eqns |> List.map(fix_eqn o)
        let vars = v |> Seq.map(fun _v -> _v.Var |> exprvar<'t>) |> Seq.toList
        Algebra.solve_for o vars (_eqns |> List.map sexpr) |> List.map scalar_varmap<'t>
        
    let solve_unique (o:'a) (x:ScalarVar<_>) (e:ScalarEquation<_> list) =
        let s = solve o [x] e
        if s.Length > 1 then failwithf "The equation %A has more than 1 solution for %A." e x
        s.[0].Rhs

    let eliminate (o:'a) (v:seq<ScalarVar<_>>)  (eqns: ScalarEquation<_> list) =
        let _eqns = eqns |> List.map(fix_eqn o)
        let vars = v |> Seq.map(fun _v -> _v.Var |> exprvar<_>) |> Seq.toList
        Algebra.eliminate o vars (_eqns |> List.map sexpr) |> List.map Scalar

    let solve_for_elim (v:ScalarVar<_>) (e:ScalarVar<_> list) (eqns: ScalarEquation<_> list) =
         let vars = e@[v] |> Seq.map(fun _v -> _v.Var |> exprvar<_>) |> Seq.toList
         Algebra.eliminate defaults vars (eqns |> List.map sexpr) |> List.map Scalar 

    let solve_for_elim_single (v:ScalarVar<_>) (e:ScalarVar<_> list) (eqns: ScalarEquation<_> list) =
        match solve_for_elim v e eqns with
        | s::[] -> s
        | [] -> failwithf "Could not solve the specified equation system for %A by eliminating %A" v e
        | l -> failwithf "Multiple expressions returned attemting to solve the specified equation system for %A by eliminating %A: %A" v e l

    let solve_for (v:ScalarVar<_>) (e:ScalarVar<_> list) (eqns: ScalarEquation<_> list) =
        let allvars = List.map (sexpr >> get_vars) eqns |> List.concat |> List.map (exprvar<_> >> ScalarVar<_>) |> List.distinct
        let evars = List.except (e@[v]) allvars
        let vars = evars@[v] |> Seq.map sexpr |> Seq.toList
        Algebra.eliminate (defaults) vars (eqns |> List.map sexpr) |> List.map Scalar 

    let solve_for_single (v:ScalarVar<_>) (e:ScalarVar<_> list) (eqns: ScalarEquation<_> list) =
           match solve_for v e eqns with
           | s::[] -> s
           | [] -> failwithf "Could not solve the specified equation system for %A in terms of %A" v e
           | l -> failwithf "Multiple expressions returned attempting to solve the specified equation system for %A in terms of %A: %A" v e l
        