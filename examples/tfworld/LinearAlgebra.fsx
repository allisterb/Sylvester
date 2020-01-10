#load "Include2.fsx"

open Sylvester.tf

defaultGraph <- new Graph<3, 2>()

let x = new Vec<12, FLOAT>("fpp")
//let r = new Vector<one, int>("ff")