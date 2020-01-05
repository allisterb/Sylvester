# Sylvester.tf
.. image:: https://mybinder.org/badge_logo.svg
 :target: https://mybinder.org/v2/gh/allisterb/Sylvester.git/master?filepath=notebooks%2FSylvester.tf.ipynb

## About
Sylvester.tf is a high-level functional and verifiable TensorFlow 2.0 API designed to embrace the overall Sylvester language goals of safety, expressiveness and interoperability. 
```fsharp
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.tf

// Create a new TF tensor graph with 4 inputs and one output and set as default graph for this program scope
let g = TensorGraph<n<4>, n<1>>()
defaultGraph <- g

// The default graph is typed according to how many inputs and output are specified.
// Type-level comparisons can be done on numeric graph properties at compile-time.
let gt = g.Inputs +< three // Has type Sylvester.Bool+True
let lt = g.Outputs +> five // Has type Sylvester.Bool+False

// Graph and element type-level properties can be verified at compile-time
check(g.Inputs +== five) //Causes type error

// We can create graph elements with types that depend on their dimensions
let m0 = Mat<dim<4>, dim<3>>("m") // Creates a TensorFlow placeholder node and output edge with shape 4x3 and name m_0.

// We can also verify the size of the matrix and other tensor dimensions at compile-time
check(m0.Dim0 +> five) //Causes type error

// Use a new TensorFlow name scope
use x = scope "x"
let a = Mat<dim<100>, dim<50>>("a")
a.Name // Name is "x/a_0"

//Revert to root scope
ends x

// Matrix operations type-checked at compile-time
let m1 = Mat<dim<4>, dim<3>>("m") //Placeholder m_1
let m2 = Mat<dim<3>, dim<7>>("m") //Placeholder m_2

let sum1 = m0 + m1 // Create a TF Add op node with input edges from m0 and m1 

let sum2 = m1 + m2 // Type error: matrices not conformable for element-wise addition

let prod1 = m1 * m2 // Create a TF MatMul node with input edges from m1 and m2

let prod2 = m2 * m1 // Type error: matrices not conformable for multiplication in this order
```