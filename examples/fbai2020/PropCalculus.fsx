#load "Include.fsx"

open Sylvester
open PropCalculus
open PredCalculus

let x = var<bool>
let P = pred<bool>

<@ forall' x (P x )= (true ==> P x)  @> |> expand
proof pred_calculus <@ forall' x (P x )= (true ==> P )  @> [
    //inst' <@ x @> <@ (P x) @> |> L
] 
//<@ P @> 

//<@ pred<bool> @>
let m = Parser.parse<bool> "P(x)"

//Parser.testn "P(x)"

//let pred<'t> = fun x -> Unchecked.defaultof<'t>
<@ let P = pred<bool> in P x @> 