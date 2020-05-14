#load "Include.fsx"

open Sylvester
open PropCalculus

let p,q,r,s = var4<bool> 
let p',q',r',s' = <@ p @>, <@ q @>, <@ r @>, <@ s @>

let ``3.76`` = proof prop_calculus <@ p |&| q ==> p @> [
    def_implies' |> LR
    distrib_or_and p' p' q' |> CommuteL |> L
]