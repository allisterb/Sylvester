#load "Include.fsx"

open Sylvester
open Sylvester.CAS

open System
open System.Collections.Generic
open System.Linq
open System.Reflection
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open Dimension
open MicroEconomics
open LinearEquations
open FunScript

MathNet.Symbolics.Infix.parse "2.0*r+a" 
do Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"

let r, s = realvar2 "r" "s"
let a, b = realconst2 "a" "b"
let f = realfun (r***2 + r*a + a)

(diff r f).Body 
   
Maxima.last_output 10

let P, Q, J = realvar "P", realvar "Q", realvar "J"

simplify ( P * (Q + Q))

Maxima.last_output(50)
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


let x, y = realvar2 "x" "y"

let i = indexvar "i"

//<@ fun (a,b) -> a + b @>
//y.[i + 1] == A * y.[i] + 2 |> sexpr
//sys |> List.map sexpr

J == P *** 2 + Q - A

let U1 = utilfun2 (U .= a * x *** 4 + 4 * y *** 3) 

U1.ScalarMapExpr

let rec make_JS_compat    = 
    function
    | SpecificCall <@@ ( ** ) @@> (_, _, [xt; yt])  -> 
        let xxt, yyt = make_JS_compat xt, make_JS_compat yt
        <@@ FunScript.Arithmetic.MathJS.Pow((%%xxt:float), (%%yyt:float)) @@>
    | expr -> traverse expr make_JS_compat

compile <| make_JS_compat U1.ScalarMapExpr


