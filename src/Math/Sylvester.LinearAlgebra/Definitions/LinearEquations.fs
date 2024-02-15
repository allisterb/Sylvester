namespace Sylvester

open FSharp.Quotations

open Sylvester.CAS

module LinearEquations =
    let solve_for (options:'a) (vars:ScalarVar<'t> list) (eqns: ScalarEquation<'t> list) =
        Algebra.solve_for options (vars |>List.map sexpr) (eqns |> List.map sexpr) 
        |> List.map scalar_varmap<'t>
    
    let solve (options:'a) (eqns: ScalarEquation<'t> list) =
        let vars = 
            eqns 
            |> List.map sexpr 
            |> List.collect get_vars 
            |> List.distinct
            |> List.map (fun v -> ScalarVar<'t> v.Name)
        Algebra.solve_for options (vars |> List.map sexpr) (eqns |> List.map sexpr) |> List.map scalar_varmap<'t>
        