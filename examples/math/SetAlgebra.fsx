#load "Include.fsx"

open Sylvester
open SetAlgebra

let A,B = var2<Set<obj>>
proof set_algebra <@ (A |+| B) = (A |+| B)@> []
