#load "Include.fsx"

open Sylvester
open PropCalculus
let p,q,r = var3<bool>

let p1 = proof <@p == q == q == p @> S [
    LR RightAssoc
] 


<@p == q == q == p @> |> expand |> src
//proof <@U = U@> set_algebra []