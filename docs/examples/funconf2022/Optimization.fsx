#load "IncludeSolver.fsx"

open Sylvester
open LP
let x = LatinVars.x<rat>
let y = LatinVars.a<rat>

let bb = max false  <@[ 6* %x + 5 * %y = 45Q; %x > 0Q; %y > 0Q; ]@> <@ %x * %x * %y @>

match bb with
| Some (RatLPSol r) -> r
| _  -> []
//let A, x , y = var3'<int> "A" "x" "y"

