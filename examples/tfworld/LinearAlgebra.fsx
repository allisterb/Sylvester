#load "Include2.fsx"

open Sylvester.tf

defaultGraph <- new Graph<3, 2>()
let x = new Vec<19, INT8>("fpp")
let y = new Vec<16, DOUBLE>("fp")
//let z = x + y
