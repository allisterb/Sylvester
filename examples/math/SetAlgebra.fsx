#load "Include.fsx"

open Sylvester
open SetAlgebra

let A,B = var2<Set<obj>>
let p =proof set_algebra <@ (A |+| Empty) = A@> [
    ident_set |> LR    
]
//p.LastState |> expand_right

//<@ Empty @> |> expand