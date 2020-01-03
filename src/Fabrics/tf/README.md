# Sylvster.tf
## About
Sylvester.tf is a high-level functional and verifiable TensorFlow 2.0 API designed to embrace the overall Sylvester language goals of safety, expresiveness and interoperability. 
```fsharp
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.tf

// Create a new tensor graph with ten inputs and one output and set as default graph for this program scope
let g = TensorGraph<n<10>, n<1>>()
defaultGraph <- g

// The default graph is typed according to how many inputs and output are specified
let inputs = g.Inputs.Length
let outputs = g.Outputs.Length

// Type-level comparisons can be done on numeric graph properties at compile-time
let gt9 = outputs +> nine
let lt5 = outputs +< five

// Graph and element type-level properties can be verified at compile-time

// Type errors are displayed in the IDE immediately as the user is editing

// We can create tensors with types that depend on their dimensions
let m0 = Mat<dim<4>, dim<3>>("")
m0.Dim0

// We can also verify the size of the matrix and other tensor dimensions at compile-time
check(m0.Dim0 +> five)

// Use a new scope
use x = scope "x"
let m1 = Mat<dim<100>, dim<50>>("m")
m1.Name

//Revert to root scope
let m2 = Mat<dim<100>, dim<50>>("m")
m2.Name
```
