#load "Include.fsx"

open Sylvester

let ee = Term 4

let r = ee - 5

let v = Term.var6<real> "v"

let q = Term.var6<rat> "q"

q ** 4Q 