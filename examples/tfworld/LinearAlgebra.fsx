#load "Include.fsx"

open Sylvester.tf

defaultGraph <- new Graph<6, 2>()

let x = Vec<d<19>>("fpp")
let m0 = Mat<d<12>, d<17>>("m")
let m1 = Mat<d<17>, d<20>>("m")
let m2 = Mat<d<12>, d<17>>("m")

let r = m0 * m1
let jj = m0 + m2
