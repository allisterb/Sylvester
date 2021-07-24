#load "IncludeMath2.fsx"

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Reflection

open Sylvester
open Dimension
open Matrix

let x = 0 
and y = x


let inline length(List l) = 

<@ (>>) @> |> expand
let yy(t:Expr<_*_*_>) = t.ToString()

yy <@(4,5) @>
let z = intvar "z"

let m = MatZ<``3``, ``4``> <@[ [%z;4;-1;2]; [0;2;1;3]; [-2; 1; -2; 2] ]@>
let n = MatZ<``4``, ``2``>.ofCols <@[[%z; 0; 2; 3]; [-1;2;1;-2]]@>

let nn = n |+|| VecZ<``4``> <@[3; 6; 9; 12]@>

diag nn

let i = raddmul m zero one <@ 4 @>

let s (x:int) = <@ x @>

s 5
let mv = MatZ<three, three> <@[[-1;4;-1]; [0;2;1]; [-2; 1; -2]]@>
let v =  vvars<three, int> "x"
let dd = m * n

let mm = Mat<two, five>.One

m * mm

let mmm = Mat<three, four> a
let mmmm = Mat<four, two> <@ %a + 2. @>

msimplify <| mmm * mmmm

mident<four, int>