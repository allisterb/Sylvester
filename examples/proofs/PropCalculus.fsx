#load "Include.fsx"

open Sylvester
open PropCalculus

/// true = (p = p)
let p,q,r,s = var4<bool>
let p',q',r',s' = <@ p @>, <@ q @>, <@ r @>, <@ s @>

let def_true p = ident prop_calculus <@ true = (%p = %p) @> [
    DefTrue |> L
]