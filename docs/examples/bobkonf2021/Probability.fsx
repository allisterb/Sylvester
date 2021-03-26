#load "IncludeMath.fsx"

open Sylvester

let urn = finite_seq [1..5] <*> finite_seq [1..4]
let S = prob_space urn
let P = prob_measure S
let E1 = urn |>| (fun s -> fst s = 5)
let E2 = urn |>| (fun s -> snd s < 4)
let E3 = urn |>| (fun s -> fst s + snd s >= 8)
P(E1)
//P(E1 |/| S) + P(E1) a