#load "Include.fsx"

open Sylvester.tf
open Sylvester.Arithmetic

defaultGraph <- new Graph<six, two>()


let x = Vec<dim<19>>("fpp")
let m0 = Mat<dim<12>, dim<17>>("m")
let m1 = Mat<dim<17>, dim<20>>("m")
let m2 = Mat<dim<12>, dim<17>>("m")

let r = m0 * m1
let jj = m0 + m2

let t = Vec<dim<2>, int>("x")
let p = Vec<ten, float>("d")