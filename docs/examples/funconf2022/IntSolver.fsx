#load "IncludeSolver.fsx"

open Sylvester
open Z3

let z3 = new Z3Solver()

let x, y = intvar2 "x" "y"


let m = Option.get <| check_sat_model z3 <@[%y = %x + 1; forall' %y (%y <= 0 ==> (%x > %y))]@>

m[z3, y]