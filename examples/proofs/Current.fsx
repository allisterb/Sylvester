#load "Include.fsx"

open Sylvester
open PropCalculus

/// true = (p = p)
let p,q,r = var3<bool>


proof prop_calculus <@ (p |&| p) = p @> [
  L GoldenRule
  def_true <@ p @> |> Trn |> L
  LR RightAssoc
  idemp_or <@ p @> |> TautR |> R
] 
