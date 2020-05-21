#load "Include.fsx"

open System.Linq
open Sylvester
open SetAlgebra

let dice = seq {1..6} |> Seq

let outcomes = dice * dice

let s = outcomes |> ProbabilitySpace

[(1, 1); (1, 2); (1, 3); (1, 4); (1, 5); (1, 6)] |> Set.fromSeq |> s.Prob