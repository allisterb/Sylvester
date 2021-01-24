#load "Include.fsx"

open Sylvester
open SetAlgebra
open PredCalculus
let P x = x > 0

<@ fun x -> x + 2 @>

//expand <@ U<int> @>
let n,t = var2<int>

let Z = sequence [0.; 1.; 2.]
Z.[1]

let d = infinite_seq_gen <@ fun n (x:float) -> x + 1.0 @>  

d.[0].Expr |> src 
//d.[3] |> src
//d.Function |> src
//d.Range |> src