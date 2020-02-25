#load "Include.fsx"
open Sylph

// Define some integer formulae of interest
let F1 = F (fun x -> 2 * x + 8)
let F2 = F (fun x -> 2 * x + 3 + 5)
let F3 = F (fun x -> 3 * x + 6 + 2 * x + 4)
let F4 = F (fun x -> 5 * x + 10)

open IntegerArithmetic
//let p1 = proof (F1 == F2) integer_arithmetic [
//    right_assoc_b
//    reduce_constants_a_b
    ]

//let p2 = proof (F3 == F4) integer_arithmetic [
//    right_assoc_a 
//    commute_a_right
//    right_assoc_a 
//    left_assoc_a_right
//    reduce_constants_a_b
//]
let F6 = F (fun x y -> x + y + 6)
let F7 = F (fun x y -> x + y + 6 + 0)

integer_arithmetic |- (F6 == F7)