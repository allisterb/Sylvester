#load "Include.fsx"

open Sylvester
open Sylvester.CAS
open Algebra
open Analysis

// Init Maxima CAS
do Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"

let p,q = ratvar2 "p" "q"

binary_operands <@@ (=) @@> <@ %p = %q @>

let L = realvar "L"
let K, pK, pL = realvar3 "K" "pK" "pL"
let E = realvar "E"

let solK = solve_for K <@[%pK * %K + %pL * %L = %E]@>


let x, y = ratvar2 "x" "y"


let soly = solve_for_as_func_of x y <@[3 * %x + 5 * %y = 120Q ]@> >> real

soly 0Q

solve_for2 p q <@[%q + 5 * %p = 40Q; 2 * %q + 15 * %p = -30Q]@>


let OO = <@ sin (pi / 2Q) @> |> MathNetExpr.fromQuotation
//let gg = Nat 4

let r = pi/ 2Q


diff <@ cos (%L * 2. + sin %L) @> L |> sprinte

<@ (sin %L) ** 2. + (cos %L) ** 2. + sin %L @> |> trigsimp |> diff L |> sprinte
//sprinte <| trigexpand <@cos (6Q * pi + %x)@>

//let RRR = <@ arcsin@>

let th = GreekVars.theta<real>

<@ sin 2. * %th @> |> latex'

Symbols.TransliterateGreek