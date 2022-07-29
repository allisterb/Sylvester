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

let solK = solve K <@[%pK * %K + %pL * %L = %E]@>


let x, y = ratvar2 "x" "y"


let soly = rat<real> >> (solve_as_real_func_of x y <@[3 * %x + 5 * %y = 120Q ]@>) >> real

soly 0.

solve2 p q <@[%q + 5 * %p = 40Q; 2 * %q - 15 * %p = -20Q]@>
//let gg = Nat 4