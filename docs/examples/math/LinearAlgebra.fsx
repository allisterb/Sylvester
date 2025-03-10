#load "Include.fsx"

open FSharp.Quotations

open Sylvester

open Matrix
open LinearEquations

do CAS.Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"

fsi.PrintWidth <- 500
let l = realvar "l"

let A = sqmat [1;2;-1;4;-1;3;0;2;2;1;1;2;1;4;1;3]

let c = A - l * (identmat 4) |> det |> AlgebraOps.ratsimp == zero


mcharpoly l A

