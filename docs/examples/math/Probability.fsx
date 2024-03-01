#load "Include.fsx"

open Sylvester


let dice = finite_seq [1..6]

(dice * dice)
let S = prob_space (dice * dice)
let P = S.Measure
//let comp = S.Set.Difference  
let A = finite_seq [for i in 1..6 -> (1, i)] 
P(A) //+ P(comp A)

let po = poisson 2.8

po.[3]

po </ 0. 

let b = bernoulli 0.4

cprob b 5
//expectation b