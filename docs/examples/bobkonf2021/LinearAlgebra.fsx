#load "IncludeMath2.fsx"

open Sylvester
open Dimension
open Matrix

let m = Mat<three, two>.One

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