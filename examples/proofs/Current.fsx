#load "Include.fsx"

open Sylvester
open PropCalculus

let p,q,r,s = var4<bool> 
let p',q',r',s' = <@ p @>, <@ q @>, <@ r @>, <@ s @>
let i,j,k = var3<int>
[<Formula>]
let x = (!!) [i, j, k] (i > 0)  (i + j > 5) 

<@ x ||| p@> |> expand
//let t = (!?)  p' <@ p 