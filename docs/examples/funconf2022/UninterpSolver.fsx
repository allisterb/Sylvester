#load "IncludeSolver.fsx"
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open Sylvester
open Z3

let z3 = new Z3Solver()

type Diamond = Diamond

let f = func<Diamond, Diamond>
let x = uninterp_var<Diamond> "x"

check_sat_model z3 <@[f(f(%x)) = %x; f(f(f(%x))) = %x]@>
