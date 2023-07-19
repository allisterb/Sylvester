#load "Include.fsx"

open FSharp.Quotations

open Sylvester
open Sylvester.CAS
open Dimension
open Vector
open Matrix

//5 |?| Zpos
// Init Maxima CAS
//do Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"


let p, q = realvar "p", realvar "q"


let P = vec3 p.[0] p.[1] p.[2]

let Q = vec3 q.[0] q.[1] q.[2]

P * Q

//sprintel [(p + q == 5.).Expr; (p - q == 1.).Expr]
//LinearEquations.solve_for p [p + q == 5.]


let J = mat ``2`` ``2``  [p.[0]; p.[1]; p.[2]; p.[1]]

J * J