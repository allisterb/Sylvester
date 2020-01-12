#load "Include2.fsx"

open Sylvester.tf

defaultGraph <- new Graph<6, 2>()

let x = new Vec<19, FLOAT>("fpp")
//let y = new Vec<19, FLOAT>("fp")
//let m = new Mat<4, 5>("z")
let m0 = Matrix<d<12>, d<17>>("m")
let m1 = Matrix<d<17>, d<20>>("m")
let m2 = Matrix<d<12>, d<17>>("m")
//y.TensorGraph.NameScope
//let z = x + y
let r = m0 * m1
let jj = m0 + m2
//let x = Vector