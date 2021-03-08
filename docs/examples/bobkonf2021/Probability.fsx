#load "IncludeMath.fsx"

open Sylvester

let gg = {| Diameter = 0; Area = 1; Circumference = 2 |}
gg
let seq = sequence
let dice = seq [1..6]
dice.['2']

let S = ProbabilitySpace(dice * dice)
let P = S.Measure
let comp = S.Set.Difference  
let A = Seq [for i in 1..6 -> (1, i)] 
P(A) + P(comp A)