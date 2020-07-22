#load "Include.fsx"

open Sylvester
open SetAlgebra

let n = var<int>
let A,B = var2<Set<obj>>

let X = Seq [for i in 0..10 -> i]
//let Y = SetComprehension(n * 3, fun o -> (o :?> int) > 5)
//set_algebra |- <@ (A |+| Empty) = A @>
let range (Z:Set<'t>) = Z.Range

let jj = <@ forall n (%X.Range(n)) ((n) = X.Body n) @> |> expand