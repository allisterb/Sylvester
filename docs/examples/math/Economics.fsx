#load "Include.fsx"

open Sylvester
open Sylvester.CAS

open Dimension
open Economics

open FunScript
open System.Linq
open RealNumbers

fsi.PrintWidth <- 500


do Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"

let Y = realvar "Y"

//solve defaults [Y] [(e *** Y) == e *** 4R]
let alpha = realconst "alpha"
let gamma = realconst "gamma"

let m = econ_model<MalthusianGrowth>()
do
    m.g <- realfun "g" (m.c + gamma)
    m.F <- prodfun2 "F" ((m.L + alpha) * (m.N + 1 - alpha))

//m.Equations

solve_model defaults m