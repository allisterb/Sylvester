namespace Sylvester

open FSharp.Quotations

open Sylvester.CAS

module LinearEquations =
    let solve_for (vars:ScalarVar<_> list) (eqns: ScalarEquation<_> list) =
        Algebra.solve_for_n (vars |>List.map sexpr) (eqns |> List.map sexpr) |> List.map Scalar<_>

    let solve  (eqns: ScalarEquation<_> list) =
        Algebra.solve_for_n (eqns |> List.map sexpr |> List.collect get_vars |> List.map (Expr.Var >> expand_as<real>)) (eqns |> List.map sexpr) |> List.map Scalar<_>
        