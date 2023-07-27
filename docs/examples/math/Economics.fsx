#load "Include.fsx"

open Sylvester
open Sylvester.CAS
open MicroEconomics
open LinearEquations

do Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"
let P, Q = realvar "P", realvar "Q"
let A = realconst "A"
//P + Q == 5. |> sexpr
let Qs = demand (P == 2 * Q + 4 + A)

//Qs.[0.]

 
let sys = [
    P + Q == 4.
    P - Q == 0.
]

solve sys
//sys |> List.map sexpr



//Sylvester.CAS.Algebra.solve_for_n [(P.Expr); (Q.Expr)] (sys |> List.map sexpr)