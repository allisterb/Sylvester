namespace Sylvester

open FSharp.Quotations

open Sylvester.CAS

module LinearEquations =
    let solve_for (vars:ScalarVar<'t> list) (eqns: ScalarEquation<'t> list) =
        Algebra.solve_for_n (vars |>List.map sexpr) (eqns |> List.map sexpr) 
        |> List.map Scalar<'t> 
        |> List.zip vars
        |> List.map(fun (v, e) -> ScalarVarMap<'t> (v, e))

    let solve (eqns: ScalarEquation<'t> list) =
        let vars = 
            eqns 
            |> List.map sexpr 
            |> List.collect get_vars 
            |> List.distinct
            |> List.map (fun v -> ScalarVar<'t> v.Name)
        Algebra.solve_for_n (vars |> List.map sexpr) (eqns |> List.map sexpr) 
        |> List.map Scalar<'t> 
        |> List.zip vars
        |> List.map(fun (v, e) -> ScalarVarMap<'t> (v, e))
        