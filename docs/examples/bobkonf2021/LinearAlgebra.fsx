#load "IncludeMath2.fsx"

open Sylvester
open Dimension
open Vector
open Matrix


let tt = 2

<@ tt + 2 @> |> expand
let z = var'<int> "z"

let m = MatZ<three, four> <@[[%z;4;-1;2]; [0;2;1;3]; [-2; 1; -2; 2]]@>
let n = MatZ<four, two>.ofCols <@[[%z; 0; 2; 3]; [-1;2;1;-2]]@>

let mv = MatZ<three, three> <@[[-1;4;-1]; [0;2;1]; [-2; 1; -2]]@>
let v =  vvars<three, int> "x"
let e = mv |+|| v


//mdiag n



let m1 = Mat<four, two>.One

let md = mdiag m

let a = LatinVars.a<real>
let v = Vec<two> <@[%a; %a]@>

let dd = m * v

let mm = Mat<two, five>.One

m * mm

let mmm = Mat<three, four> a
let mmmm = Mat<four, two> <@ %a + 2. @>

msimplify <| mmm * mmmm

mident<four, int>