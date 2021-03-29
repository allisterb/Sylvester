#load "IncludeMath.fsx"

open Sylvester
let urn = sseq [1..5] * sseq [1..4]
measure urn
urn.Cardinality.Measure()
urn.Cardinality

let ff = infinite_seq <@ fun n -> 55 + n @>

ff.[100u]


let S = prob_space urn
let P = prob_measure S
let E1 = urn |>| (fun s -> fst s = 5)

let E2 = urn |>| (fun s -> snd s < 4)
let E3 = urn |>| (fun s -> fst s + snd s >= 8)
card E2
//P(E1 |/| S) + P(E1) a


