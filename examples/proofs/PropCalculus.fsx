#load "Include.fsx"

open Sylvester
open PropCalculus

// Declare some variables we can reuse in formulae
let p,q,r = var3<bool>

<@ p = p = q = q @> |> EquationalLogic.right_assoc |> EquationalLogic.right_assoc |> expand_left |> Option.get |> src