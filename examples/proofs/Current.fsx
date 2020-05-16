#load "Include.fsx"

open Sylvester
open PropCalculus

let p,q,r,s = var4<bool> 
let p',q',r',s' = <@ p @>, <@ q @>, <@ r @>, <@ s @>

let x = <@ p ||| q ||| not q @>

replace_var "p" <@ true@> x

