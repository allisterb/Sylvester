#load "Include.fsx"

open Sylvester
open Sylvester.CAS
open Algebra

// Init Maxima CAS
do Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"

let p,q = intvar2 "p" "q"

let L = realvar "L"
let K, pK, pL = realvar3 "K" "pK" "pL"
let E = realvar "E"

let solK = solve K <@[%pK * %K + %pL * %L = %E]@>
sprint' solK
let x, y = ratvar2 "x" "y"
sprint' <| solve y <@[3 * %x + 5 * %y = 120Q ]@>
