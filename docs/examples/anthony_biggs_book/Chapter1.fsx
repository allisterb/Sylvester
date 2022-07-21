#load "Include.fsx"

open Sylvester
open Z3

let z3 = new Z3Solver()

let p,q = intvar2 "p" "q"

get_int_var_sol z3 <@[%q + 5 * %p = 40]@> p