#load "Include.fsx"

open FSharp.Quotations

open Sylvester

open Matrix
open LinearEquations

do CAS.Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"

fsi.PrintWidth <- 500

MathNet.Symbolics.Infix.parseMatrix("matrix([1,-1,1,1],[-1/9,-1,0,0],[-13/9,1,-1,0],[17/9,-1,1,1]") |> Result.map(List.map(List.map (MathNetExpr.toQuotation<real> [])))
let A = sqmat [2;0;1;-3;0;2;10;4;0;0;2;0;0;0;0;3]
let J = jordan_normal_form A |> perm_jordan_blocks [1;0;2]
J |> jordan_blocks |> Array.map(fun b -> b.[0,0].Expr, exprv b.Dims.[0])  |> Array.map(fun (e,n) -> sprintf "[%s,%s]" (sprinte e) (sprinte n)) |> Array.reduce (sprintf "%s,%s") |> sprintf "[%s]"
let P = jordan_similar J A 
P |> mexpr
(P * P)
(inverse P) * A * P = J

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

