#load "IncludeMath.fsx"

open Sylvester

let pp = poisson 3.

pp.Prob <| 4

let urn = sseq [1..5]  

let S = prob_space (urn * urn)

card S

let P = prob_measure S

let E1 = S |>| (fun s -> fst s = 5)

let E2 = S |>| (fun s -> snd s < 4)
let E3 = S |>| (fun s -> fst s - snd s >= 8)


P (E1 |/| S)

type Coin = | Heads | Tails

let coin = sseq [Heads; Tails]

let dice = sseq [1..6]

let SS = prob_space (coin * dice)

let E4 = SS |>| (fun (_,d) -> d = 3)

prob_measure SS E4