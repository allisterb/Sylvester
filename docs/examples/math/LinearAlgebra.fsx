#load "Include.fsx"

open Sylvester
open Sylvester.CAS
open Dimension
open Vector

// Init Maxima CAS
do Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"

3. * vec ``3`` [-2; 0; 1]

let p, q = scalar_var<real> "p", scalar_var<real> "q"

<@ p @>
let P = vec ``3`` [p.[0]; p.[1]; p.[2]]

let Q = vec ``3`` [q.[0]; q.[1]; q.[2]]

//P * Q

//sprintel [(p + q == 5.).Expr; (p - q == 1.).Expr]
//LinearEquations.solve_for p [(p + q == 5.)]