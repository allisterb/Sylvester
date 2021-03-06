#load "Include.fsx"

open Sylvester.tf

// Default graph for this program scope has 3 inputs and one output
defaultGraph <- TensorGraph<d<3>, d<1>>()

// Create 3 matrix placeholders in root scope
let m0 = Mat<d<6>, d<100>>("m")
let m1 = Mat<d<6>, d<100>>("m")
let m2 = Mat<d<100>, d<200>>("m")

// Create a matrix algebra operation in new name scope
let output = 
    use matops = scope "MatOps" //New scope
    let sum1 = m0 + m1 //Type checks ok
    //let sum2 = m1 + m2 // Type error
    sum1 * m2 //Type checks ok

// Export the graph to a protocol buffer text-format file
//defaultGraph.Export("matops.pbtxt")