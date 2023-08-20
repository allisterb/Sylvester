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

//y.[i + 1] == A * y.[i] + 2 |> sexpr
//sys |> List.map sexpr

J == (P ^^ 2) + Q - A

let JF = RealFunction2((P ^^ 2) + J - A)

JF.[y, x]
//open FSharp.Quotations.Patterns

//let gg = <@ fun (A,B) -> A ** 2. + B - 3. @>
//match gg with | Lambda (e, _) -> e.Type | _ -> failwith ""

//typeof<Vec<dim<2>>>.GetConstructor("create")
//let uvv = <@ fun (v:Vec<dim<2>>) -> v.[0] + v.[1] @>
//uvv
//JF.[0., 5.]
//Sylvester.CAS.Algebra.solve_for_n [(P.Expr); (Q.Expr)] (sys |> List.map sexpr)