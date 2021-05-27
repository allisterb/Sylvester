#load "IncludeMath2.fsx"

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Reflection

open Sylvester
open Dimension
open Vector

let z = var'<int> "z"

let v = vecz ``5`` <@ 4,5,6,7,%z @>

let u = vecz ``4`` <@ 4,5,6,7 @>

let r = vec ``3`` <@ 3., 5., 6.@>

let aa = <@(4, 5, 6, 7), (8, 19, 10)@>

let m = MatZ<``3``, ``4``> <@[ [%z;4;-1;2]; [0;2;1;3]; [-2; 1; -2; 2] ]@>
let n = MatZ<``4``, ``2``>.ofCols <@[[%z; 0; 2; 3]; [-1;2;1;-2]]@>

m.LinearDisplay

let nn = n |+|| Vector<``4``, int> <@[3; 6; 9; 12]@>


let md = m |> diag

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