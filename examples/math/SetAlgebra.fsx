#load "Include.fsx"

open Sylvester
open SetAlgebra

let n = var<int>
let A,B = var2<Set<obj>>

let X = Seq [1;2;3]

//set_algebra |- <@ (A |+| Empty) = A @>
let range (Z:Set<'t>) = Z.Range

let jj = <@ forall n (%X.Range(n)) ((n) = %X) @> |> expand