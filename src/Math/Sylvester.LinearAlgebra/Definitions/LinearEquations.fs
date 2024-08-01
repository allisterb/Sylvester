namespace Sylvester

open Sylvester
open Arithmetic
open Dimension
open Sylvester.CAS

type LinearSystem<'rank when 'rank :> Number>(eqns: seq<ScalarEquation<real>>) = 
    member val Equations = eqns


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
        