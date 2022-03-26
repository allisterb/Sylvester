#load "IncludeSolver.fsx"

open Sylvester
open Z3

let z3 = new Z3Solver()

let x, y = intvar2 "x" "y"
let Q = arrayvar<int>

check_unsat z3 <@ (forall' Q (Q.[%x] = Q.[%y])) ==> (%x = %y) @>