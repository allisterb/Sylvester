# Sylvester.tf
## About
Sylvester.tf is a high-level functional and verifiable TensorFlow 2.0 API designed to embrace the overall Sylvester language goals of safety, expresiveness and interoperability. 
```fsharp
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.tf

// Create a new tensor graph with 4 inputs and one output and set as default graph for this program scope
let g = TensorGraph<n<4>, n<1>>()
defaultGraph <- g

// The default graph is typed according to how many inputs and output are specified.
// Type-level comparisons can be done on numeric graph properties at compile-time.
let gt = g.Inputs +< three // Has type Sylvester.Bool+True
let lt = g.Outputs +> five // Has type Sylvester.Bool+False

// Graph and element type-level properties can be verified at compile-time
check(g.Inputs +== five) //Causes type error

// We can create graph elements with types that depend on their dimensions
let m0 = Mat<dim<4>, dim<3>>("m") // Creates a TensorFlow placeholder node and graph edge with shape 4x3

// We can also verify the size of the matrix and other tensor dimensions at compile-time
check(m0.Dim0 +> five) //Causes type error

// Use a new TensorFlow name scope
use x = scope "x"
let m1 = Mat<dim<100>, dim<50>>("m")
m1.Name // Name is "x/m"

//Revert to root scope
ends x
```
