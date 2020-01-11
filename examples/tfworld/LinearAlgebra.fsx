#load "Include2.fsx"

open Sylvester.tf
defaultGraph <- new Graph<3, 2>()

let x = new Vec<19, FLOAT>("fpp")
//let y = new Vec<19, FLOAT>("fp")
//let m = new Mat<4, 5>("z")
let m0 = new Mat<20, 6, INT8>("m")
let m1 = new Mat<6, 7, INT8>("m")
//y.TensorGraph.NameScope
//let z = x + y
let r = m0 * m1

//let x = Vector