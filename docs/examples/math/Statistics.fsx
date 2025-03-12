#load "Include.fsx"

open Sylvester
open SimpleLinearRegression
let y,x = realvar2 "y" "x"
let b0,b1,u = realconst3 "beta_0" "beta_1" "u"

let lm = slrm (y == b0 + b1 * x) [
    1,3
    2,5
    3,6
]
lm

lm

lm.[4]
let me (s:seq<Scalar<real>>) f = Seq.reduce f s

let J = y ** 2

//lems witht lm = SimpleLinearRegressionModel(y .= b0 + b1 * x + u + b0, [])
