#load "Include.fsx"

open Sylvester
open Sylvester.Arithmetic

let x,y = var2<int>
let v1 = Vec<dim<4>>(4.0, 3.0, 3.0, 1.0)
let v2 = Vec<dim<4>>(3.0, 2.0, 1.0, 5.0)

let e = v1 + v2

let dd x = 
    match expand x with
    | Patterns.List r -> r
    | _ -> failwith "None"

//let zz = v1 + v2

//let m1 = Mat<two, two>([2.;5.], [1.;2.])
//let m2 = Mat<two, one>([1.], [1.])

//let r = m1 + m1

let g = <@[
    3 * x + 5 = 0
    4 * x - 5 = 0
    9 * x + 1 = 0
]@>

g |> expand |> get_vars 

//let h = <@ [3 * x + 5 = 0; 4 * x - 5 = 0;9 * x + 1 = 0] @>

//dd h