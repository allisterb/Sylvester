#load "Include.fsx"

open Sylvester

let dice = finite_seq [1..6]
dice.Range

let S = ProbabilitySpace(dice * dice)
let P = S.Measure
//let comp = S.Set.Difference  
let A = Seq [for i in 1..6 -> (1, i)] 
P(A) //+ P(comp A)

let b = poisson 4.

b.[2.] 