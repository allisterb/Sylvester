#load "Include.fsx"

open Sylvester
open PropCalculus
open PredCalculus

let p,q,r,s = var4<bool>
let x,y = var2<bool> 
let p',q',r',s' = <@ p @>, <@ q @>, <@ r @>, <@ s @>
let x' = <@ x @>
let y' = <@ y @>
let i, n, m = var3<int>
let P,Q, N, A= var4<bool>
let P',Q', N' = <@ P @>, <@ Q @>, <@ N @>

(*
proof pred_calculus <@ exists x N (Q ==> P) ==> ((exists x N Q) ==> (exists x N P))@> [ 
    ident_implies_not_or Q' P' |> L 
    //distrib_exists_or' x' N' <@ not Q @> P' |> L
    //rshunt |> LR
    //commute |> L
    //distrib_and_or <@ exists x N Q @> <@ exists x N (not Q) @> <@ exists x N P @> |> L
    //commute |> L
]
*)
let pp = proof pred_calculus <@ exists x N (forall y Q P) ==> (forall y Q (exists x N P)) @> [
    def_implies |> LR
    distrib_or_forall |> L 
    collect_exists_or' x' N' <@ forall y Q P@> P' |> L
    inst <@ forall y Q P @>  y' <@ P @> |> L |> QB' |> QB' |> L'
    //trade_body |> L |> QB' |> QB' |> L'
    //
    commute |> QB |> QB' |> L'
    distrib |> QB |> QB' |> L'
    ident_or_conseq P' Q' |> R |> QB' |> QB' |> L' 
    distrib_or_forall |> L |> QB' |> QB' |> L'
    idemp_or P' |> L |> QB' |> QB' |> L'
    //idemp_or P' |> QB |> L' 

]

//let yy = pp |> last_state |> expand_left |> expand_left |> get_vars |> List.item 2

//Patterns.occurs_free [yy] (pp |> last_state |> expand_left |> expand_left)

//pp |> last_state |> expand_left |> expand_left |> src