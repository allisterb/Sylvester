#load "Include.fsx"

open Sylvester
open PredCalculus

let p,q,r,s = var4<bool>
let x,y = var2<bool> 
let p',q',r',s' = <@ p @>, <@ q @>, <@ r @>, <@ s @>


let P,Q,_R,S = var4<bool>

proof pred_calculus <@ forall x _R P = (forall' x (_R ==> P)) @> [

]
//let i,j,k = var3<int>

