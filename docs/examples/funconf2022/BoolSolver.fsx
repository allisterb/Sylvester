#load "IncludeSolver.fsx"
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open Sylvester
open Z3

let z3 = new Z3Solver()

let P = pred'<int> "P"
let p, q = boolvar2 "p" "q"


get_bool_var_model z3 <@[ exists' %p (%p ||| not %q) ]@>

let h = func<int, bool>
let hh = func<int, int>

<@ hh 4 @>

let internal (|FuncType|_|) =
    function
    | Application(PropertyGet (None, pi,  []), e) -> Some(pi, e)
    | _ -> None