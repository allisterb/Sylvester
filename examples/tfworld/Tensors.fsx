#load "Include.fsx"

open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.tf

// Create a new tensor graph with ten inputs and one output and set as default graph for this program scope
let g = TensorGraph<dim<10>, dim<1>>()
defaultGraph <- g
// The default graph is typed according to how many inputs and output are specified


// Type-level comparisons can be done on numeric graph properties at compile-time


// Graph and element type-level properties can be verified at compile-time


// Type errors are displayed in the IDE immediately as the user is editing

// We can create tensors with types that depend on their dimensions


// We can also verify the size of the matrix and other tensor dimensions at compile-time


// Use a new scope
let m0 = Mat<dim<100>, dim<60>>("m")
let m1 = Mat<dim<100>, dim<60>>("m")
// Revert to root scope


// Matrix expressions are type-checked at compile time


// Type-check matrix addition


// Matrices type-checked to be conformal for multiplication
