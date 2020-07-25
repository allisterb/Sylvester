#load "Include.fsx"

open Sylvester
open Sylvester.Arithmetic

let v1 = Vec<dim<4>>(4.0, 3.0, 3.0, 1.0)

let v2 = Vec<dim<4>>(3.0, 2.0, 1.0, 5.0)

let e = v1 + v2

let f = v1 * v2

let m1 = Matrix<two, five, float>([1.;2.;3.;4.;5.], [1.;2.;3.;4.;5.])
//let m2 = Matrix<one, five, float>([[1.;2.;3.;4.;5.]])

//m1._Matrix