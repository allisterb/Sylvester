#load "Include.fsx"

open Sylvester

let dice = Seq [1..6]
dice.Range

let S = ProbabilitySpace(dice * dice)
let P = S.Measure
let comp = S.Set.Difference  
let A = Seq [for i in 1..6 -> (1, i)] 
P(A) + P(comp A)