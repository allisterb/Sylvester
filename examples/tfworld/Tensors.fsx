#load "Include.fsx"

open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.tf

//Create a new tensor graph and set as default graph for this program scope
let g = TensorGraph<ten, one>()
defaultGraph <- g

//Many graph and element properties can be verified at compile-time
//The default graph is typed according to how many inputs and output are specified
let inputs = g.Inputs.Length
let outputs = g.Outputs.Length

//Type-level comparisons can be done on numeric graph properties at compile-time
let gt9 = outputs +> nine
let lt5 = outputs +< five

// Use a new scopea
let m0 = Mat<four, four>("")
m0.Name
use x = scope "x"
let m1 = Mat<dim<100>, dim<50>>("m1")

m1.Name


