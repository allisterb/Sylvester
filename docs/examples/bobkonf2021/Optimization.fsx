#load "IncludeMath2.fsx"

open Sylvester
open LP
let x = LatinVars.x<rat>
let y = LatinVars.a<rat>

let bb = max <@ %x * %x * %y @> <@[ 6* %x + 5 * %y = 45Q; %x > 0Q; %y > 0Q; ]@>
match bb with
| Some (RatSol r) -

let A, x , y = var3'<int> "A" "x" "y"

algebraic_expand <@ (%A * (%x + %y - %A)) @> |> sprint'
