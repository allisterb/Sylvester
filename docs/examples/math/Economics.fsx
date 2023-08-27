#load "Include.fsx"

open Sylvester
open Sylvester.CAS

open System.Reflection
open FSharp.Quotations

open Dimension
open MicroEconomics
open LinearEquations


//typeof<Vec<dim<2>>>.GetMethod("get_Item")
do Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"
let P, Q, J = realvar "P", realvar "Q", realvar "J"
let U = realvar "U"
let A = realconst "A"
//P + Q == 5. |> sexpr
let Qs = demand (P .= 2 * Q + 4 + A)

Qs.[A]

 
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

let a, b = realconst2 "a" "b"
let x, y = realvar2 "x" "y"

let i = indexvar "i"

//<@ fun (a,b) -> a + b @>
//y.[i + 1] == A * y.[i] + 2 |> sexpr
//sys |> List.map sexpr

J == (P ^^ 2) + Q - A

let U1 = utilfun2 (U .= a * x ^^ 4 + b * y ^^ 3) 

U1.Body