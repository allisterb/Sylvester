#load "Include.fsx"

open Sylvester
open PropCalculus
open Sylvester.Patterns
let p,q,r,s = var4<bool> 
let p',q',r',s' = <@ p @>, <@ q @>, <@ r @>, <@ s @>
let i,j,k = var3<int>

[<Formula>]
let x = forall (i, j, k) (i > 0)  (i + j > 5) 

<@ x ||| p @> |> expand

let (bound, range, body) = 
    match <@ x @> |> expand with
    | ForAll(a, b, c) -> (a, b, c)
    | _ -> failwith "Not a quantifier" 

body