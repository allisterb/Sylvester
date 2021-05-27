#load "IncludeMath.fsx"

open Sylvester
open Series
open Vector
open Dimension

let x, y = realvar "x", realvar "y" 

let n = realvar "n"

[<Formula>]
let F x y = Vec<``2``> <@[x + 2.; y]@> 

vexpr (F 1. 1.)
//

//let x = vec ``3`` <@ 1. / %n, 1. / %n, 2. / %n @>

norm x

