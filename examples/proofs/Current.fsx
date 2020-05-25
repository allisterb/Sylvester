#load "Include.fsx"

open Sylvester
open PropCalculus
open PredCalculus

let p,q,r,s = var4<bool> 
let p',q',r',s' = <@ p @>, <@ q @>, <@ r @>, <@ s @>
let i,j,k = var3<int>

let n = getExprName <@ PropCalculus.Symbols.p @>
let e = <@ PropCalculus.Symbols.p @>

e.CustomAttributes//open System.Reflection
//m.GetMe
//proof pred_calculus <@ sum i (i = 3) (i * i) = (3 * 3) @> []


