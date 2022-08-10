#load "Include.fsx"

open Sylvester
open Sylvester.CAS
open Algebra

// Init Maxima CAS
do Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"

let p,q = ratvar2 "p" "q"

let L = realvar "L"
let K, pK, pL = realvar3 "K" "pK" "pL"
let E = realvar "E"

let solK = solve_for K <@[%pK * %K + %pL * %L = %E]@>


let x, y = ratvar2 "x" "y"


let soly = solve_for_as_func_of x y <@[3 * %x + 5 * %y = 120Q ]@> >> real

soly 0Q

solve_for2 p q <@[%q + 5 * %p = 40Q; 2 * %q + 15 * %p = -30Q]@>


let OO = <@ sin %E @> |> MathNetExpr.fromQuotation
//let gg = Nat 4