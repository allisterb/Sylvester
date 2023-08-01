#load "Include.fsx"

open Sylvester
open Sylvester.CAS
open MicroEconomics
open LinearEquations

do Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"
let P, Q, J = realvar "P", realvar "Q", realvar "J"
let A = realconst "A"
//P + Q == 5. |> sexpr
let Qs = demand (P .= 2 * Q + 4 + A)

//Qs.[0.]

 
let sys = [
    P + 2 * Q == 4.
    P - Q == 12.
    J == 6.
]

solve sys

let p = ppf [
    P + Q +> 4.
    P == 7.
]

let y = realvar "y"

let i = indexvar "i"

y.[i + 1] == A * y.[i] + 2 |> sexpr
//sys |> List.map sexpr



//Sylvester.CAS.Algebra.solve_for_n [(P.Expr); (Q.Expr)] (sys |> List.map sexpr)