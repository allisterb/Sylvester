#load "Include.fsx"

open Sylvester

let dice = Seq [1..6]
let S = ProbabilitySpace(dice.Product)
let A = Seq [for i in 1..6 -> (1, i)] 

S.Prob A

