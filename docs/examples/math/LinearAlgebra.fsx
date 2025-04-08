#load "Include.fsx"

open System
open FSharp.Quotations

open Sylvester

open Vector
open Matrix
open LinearEquations

do CAS.Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"

fsi.PrintWidth <- 500

let x,y = realvar2 "x" "y"
(*
MathNet.Symbolics.Infix.parseMatrix("matrix([1,-1,1,1],[-1/9,-1,0,0],[-13/9,1,-1,0],[17/9,-1,1,1]") |> Result.map(List.map(List.map (MathNetExpr.toQuotation<real> [])))
let A = sqmat [2;0;1;-3;0;2;10;4;0;0;2;0;0;0;0;3]
let J = jordan_normal_form A |> perm_jordan_blocks [1;0;2]
J |> jordan_blocks |> Array.map(fun b -> b.[0,0].Expr, exprv b.Dims.[0])  |> Array.map(fun (e,n) -> sprintf "[%s,%s]" (sprinte e) (sprinte n)) |> Array.reduce (sprintf "%s,%s") |> sprintf "[%s]"
let P = jordan_similar J A 
P |> mexpr
(P * P)
((inverse P) * A * P) 

let l = realvar "l"

MathNet.Symbolics.Infix.parseLists("[[                                                                                                                                                                                                                                                                                       -1,0],[2,4]]")
let B = sqmat [1;2;-1;4;-1;3;0;2;2;1;1;2;1;4;1;3]

mcharpoly l B
//let c = A - l * (identmat 4) |> det |> AlgebraOps.ratsimp == zero

//CAS.Maxima.send

mechelon A

let a = sqmat [2;1;2;0;-2;2;1;2;-2;-1;-1;1;3;1;2;-1]
*)
let p = realconst "p"

let sys1 = [
    4*y + 3*p*x + 6*x + 7 *y + 2*x  == 0.
    3*x + 2*y == 0.
] 

let RR = augmat sys1





RR |> merops [
    mersw 1 0
    //meroadd 0 1 4.
    //meromul 0 2.
] 

let x1,x2,x3 = realvar3 "x_1" "x_2" "x_3"
let x4,x5 = realvar2 "x_4" "x_5"
let sys2 = [
    x1 + x2 + x3 == 3.
    2*x1 + x2 + x3 == 4.
    x1 - x2 + 2  * x3 == 5.
]

let sys3 = [
    x1 + x2 + x3 + x4 + x5 == 3.
    2*x1 + x2 + x3 + x4 + 2*x5 ==  4.
    x1 - x2 - x3 + x4 + x5 == 5.
    x1 + x4 + x5 == 4.
]

       
let A = augmat sys2
A |> merops [
    meradd 1 0 -2
    meradd 2 0 -1
    mermul 1 -1
    meradd 2 1 2
    mermul 2 (1R/3)
] = mref A
let B = mref A


sys3 |> augmat |> mrref
mrref <| mat [[1;1;1;3]; [2;1;1;4]; [1;-1;2;5]]