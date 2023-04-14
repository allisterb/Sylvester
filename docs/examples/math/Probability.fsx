#load "Include.fsx"

open Sylvester

let dice = finite_seq [1..6]
dice.Range

let S = prob_space (dice * dice)
let P = S.Measure
//let comp = S.Set.Difference  
let A = finite_seq [for i in 1..6 -> (1, i)] 
P(A) //+ P(comp A)

let po = poisson 2.8

po.[3.]

po </ 7. 

//let b = binomial
//expectation b