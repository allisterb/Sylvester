#load "IncludeMath.fsx"

open Sylvester
open Series
let x = LatinVars.x<rat>

[<Formula>]
let f x = x ** 2Q

src <| integrate <@ f @> x 

let a = LatinVars.a<real>
geometric_series' a |> take 3