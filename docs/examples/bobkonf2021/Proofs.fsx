#load "IncludeProver.fsx"

open Sylvester
open PropCalculus
open PredCalculus


let x, y = var2'<int> "x" "y"
let f(x, y) = 0 

sprint' <@ f(%x, %y)  = f(%y, %x) @>

let p, q = boolvar2 "p" "q"

let x, Q, N, P = boolvar4 "x" "Q" "N" "P"

do Proof.LogLevel <- 1
let t1 = theorem pred_calculus <@ forall %x (%Q |&| %N) %P = (forall %x %Q (%N ==> %P)) @> [
        trade_forall_implies x <@ %Q |&| %N @> P |> apply_left
        shunt |> apply_body |> after_left
        trade_forall_implies x Q  <@ %N ==> %P @> |> Commute |> apply_left
]

theorem prop_calculus <@ (%p = %q) = (%p |&| %q) ||| (not %p |&| not %q) @> [
        apply_right collect
        commute |> apply_left |> after_left
        commute |> apply_left
        commute |> apply_right |> after_left
        golden_rule' p q |> LeftAssoc |> apply_left
]