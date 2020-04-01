#load "Include.fsx"

open Sylvester
open PropCalculus
let p,q,r = var3<bool>


let p1 = proof <@ not p == q == p == q @> S [
    //eq_id_lr <@ true == (p == p) @>
] 
