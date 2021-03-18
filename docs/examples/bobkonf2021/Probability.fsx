#load "IncludeMath.fsx"

open Sylvester
[<Formula>]
let dice = Seq [1..6]

let urn = sseq2 [1..5]
urn.[0]
seq_length urn
let j = urn |>| (fun x -> fst x = 1)

seq_length j

let p = prob_space urn

let P = p.Measure

P j
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