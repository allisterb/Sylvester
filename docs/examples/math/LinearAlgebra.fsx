#load "Include.fsx"

open FSharp.Quotations

open Sylvester

open Matrix
open LinearEquations

do CAS.Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"

fsi.PrintWidth <- 500


let A = sqmat [2;0;1;-3;0;2;10;4;0;0;2;0;0;0;0;3]
A |> jordan_normal_form |> mjordan_blocks


let l = realvar "l"

MathNet.Symbolics.Infix.parseLists("[[-1,0],[2,4]]")
let B = sqmat [1;2;-1;4;-1;3;0;2;2;1;1;2;1;4;1;3]

mcharpoly l B
//let c = A - l * (identmat 4) |> det |> AlgebraOps.ratsimp == zero

//CAS.Maxima.send

mechelon A

let a = sqmat [2;1;2;0;-2;2;1;2;-2;-1;-1;1;3;1;2;-1];
mjordan_blocks <| jordan_normal_form a

CAS.Maxima.last_output 24

