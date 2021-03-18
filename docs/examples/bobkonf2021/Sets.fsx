#load "IncludeMath.fsx"

open Sylvester
open Sylvester.Arithmetic

let P = new Nat<12>()

let x = var<int>
let z = set <@ x > 5 @> <@ x + 2 @> 
z.[0]
let zz = sseq [0; 4; 2]
zz.[1]

let dd = FiniteSet<Nat<20>, int>([5])
