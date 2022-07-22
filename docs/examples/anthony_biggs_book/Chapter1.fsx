#load "Include.fsx"

open Sylvester
open Sylvester.CAS
open Algebra

// Init Maxima CAS
do Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"

let p,q = intvar2 "p" "q"

let K, _L, pK, pL = realvar4 "K" "L" "pK" "pL"
let E = realvar "E"

sprint' <@[%q + 5 * %p = 40]@> 

sprint' <| solve K <@[%pK * %K + %pL * %_L = %E]@>

Maxima.defaultInt.Value.ConsoleSession.Last10Output
let ee =  solve K <@[%pK * %K + %pL * %_L = %E]@> 
let eee =  (ratexpand <| ee)

sprint' <| subst eee _L <@ 0.0 @> 
//MathNetExpr.fromQuotation ee