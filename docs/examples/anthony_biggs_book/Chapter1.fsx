#load "Include.fsx"

open Sylvester
open Sylvester.CAS
open Algebra


// Init Maxima CAS
do Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"

open Algebra 
let p,q = intvar2 "p" "q"

let L = realvar "L"
let K, pK, pL = realvar3 "K" "pK" "pL"
let E = realvar "E"

solve K <@[%pK * %K + %pL * %L = %E]@>

let x, y = intvar2 "x" "y"
let soln = solve y <@[3 * %x + 5 * %y = 120 ]@>
let xx = MathNet.Symbolics.Infix.parse "-(3*x-120)/5"

xx |> function | Ok e -> Some <| MathNetExpr.toQuotation<int> (get_vars x) e | _ -> None

Maxima.last_output()