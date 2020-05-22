#load "Include.fsx"

open Sylvester

let dice = seq {1..6} |> Seq

let outcomes = dice * dice

let s = outcomes |> ProbabilitySpace

let P = s.Prob
let A = [(1, 1); (1, 2); (1, 3); (1, 4); (1, 5); (1, 6)] |> Set.fromSeq
let B = [(2, 1); (2, 2); (3, 3); (4, 4); (5, 5); (6, 6)] |> Set.fromSeq

P(A |+| B)