#load "IncludeMath.fsx"

open Sylvester

let dice = sseq2 [1..6]
let S = prob_space (dice)
let P = prob_measure S
let A = sseq2 [1..3]
P(A)



let dd = dice * dice

let S = ProbabilitySpace (dd)

//Seq.length <| { for i in 1 .. 10 -> i * i }
//dice.[2u]

let S = ProbabilitySpace (dice * dice)
let P = S.Measure
let comp = S.Set.Difference
let A = sseq2 [5..6]
P(A)

A |+| (dice *  dice) |> seq_length

open FSharp.Quotations
type TS =
| Foo of Expr<int>

let 
let p = set 
p