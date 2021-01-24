#load "MathInclude.fsx"

open Sylvester
open SetAlgebra
let A,B,C = var3<Set<any>>


let p1 = proof set_algebra <@ A |+| A = A @> [
    //eq_id_lr <@ true == (p == p) @>
] 
