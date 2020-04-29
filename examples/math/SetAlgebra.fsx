#load "Include.fsx"

open Sylvester
open SetAlgebra

let A,B,C = var3<Set<obj>>

proof set_algebra <@ A |+| B = (B |+| A) @> []