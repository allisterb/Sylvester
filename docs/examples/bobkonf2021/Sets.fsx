#load "IncludeMath.fsx"

open Sylvester
open Sylvester.Arithmetic
open SetAlgebra
let A = var<SetFamily<int>>
let i, n = var2<int>
let dd = proof set_algebra <@ (intersect i (i < n) A.[i]) = Empty@> []

//Display.print_formula (<@ (intersect i (i < n) A.[i]) @>)
let P = new Nat<12>()

let x = var<int>
let z = set <@ x > 5 @> <@ x + 2 @> 
z.[0]
let zz = sseq [0; 4; 2]
zz.[1]

let dd = FiniteSet<Nat<20>, int>([5])
