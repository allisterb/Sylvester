#load "Include.fsx"

open Sylvester

let x = LatinVars.x<int>

let B = countable_infinite_set  <@ %x > 0@> <@ 2 * %x @> 