#load "IncludeMath.fsx"

open Sylvester
open Dimension

MathNetExpr.toQuotation (Expr.VMathMathNetMathNet.Symbolics.Infix.parse "0.3333333333333333*x"


let X = binomial 0.5 6 

let sx = 4 - X

prob sx 3 = prob X 1
expectation X
expectation sx = 4. - expectation X
sx.Distribution.Func |> src
prob X 1

let U = uniform_continuous 2. 5.


probi (U + 1) (2.1) 3.
expectation sx

let Y = binomial<Coin> 0.7 6 
expectation Y

let U = uniform_continuous<Coin> 1 4

probi U 0 0.5

prob_expr sx xx |> sexpr |> sprint'

let a =  prob_expr X xx


let b = prob_expr X <@ %xx - 1.@>
a - b
type WCGoals = int array

let pp = uniform<WCGoals> [1. .. 10.]

cprob pp 3.

expectation pp



let Y = 6. - X
X.Distribution.Expr
Y.Distribution.Expr
prob Y 2

sexpr <| expectation pp 

pp.Prob <| 4.


type Coin = | Heads | Tails

let coin = sseq [Heads; Tails]

let dice = sseq [1..6]

let SS = prob_space (coin * dice)

let E4 = SS |>| (fun (_,d) -> d = 3)

prob_measure SS E4
let urn = sseq [1..5]  

let S = prob_space (urn * urn)

card S

let P = prob_measure S

let E1 = S |>| (fun s -> fst s = 5)

let E2 = S |>| (fun s -> snd s < 4)
let E3 = S |>| (fun s -> fst s - snd s >= 8)


P (E1 |/| S)

