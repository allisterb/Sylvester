#load "Include2.fsx"

open Sylvester.tf
open TensorFlow
defaultGraph <- new Graph<3, 2>()

let x = new Vec<19, UINT8>("fpp")
let y = new Vec<19>("fp")
//y.TensorGraph.NameScope
let z = x + y
//let x = Vector