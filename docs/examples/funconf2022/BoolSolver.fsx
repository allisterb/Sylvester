#load "IncludeSolver.fsx"
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open Sylvester
open Z3

let z3 = new Z3Solver()


let P = pred'<int> "P"
let p, q = boolvar2 "p" "q"

let x,y = intvar2 "x" "y"

get_bool_var_model z3 <@[ exists' %p (%p ||| not %q) ]@>

let h = func<int, bool>
let hh = func<int, int>

let m = check_sat_model z3 <@[ (hh(hh 4)) <> (hh 4) ]@>
m.Value.[z3,  <@ hh 4 @> ]


let j:int[] = [|0|]

let A = arrayvar<int> 

<@ A[%x] <-- %y @>


check_sat_model z3 <@[ A[%x] = %x; (A[%x] <- %y) = %x]@>
