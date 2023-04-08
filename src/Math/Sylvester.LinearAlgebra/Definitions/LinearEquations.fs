namespace Sylvester

open Sylvester.CAS

module LinearEquations =
    let solve_for (x:ScalarVar<_>) (eqns: Scalar<bool> list) =
        Algebra.solve_for x.Expr (eqns |> List.map sexpr)
