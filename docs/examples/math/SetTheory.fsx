#load "Include.fsx"

open FSharp.Quotations

open Sylvester
let j:Expr<Set<int>> = <@ Set.Empty |+| Set.Empty @>

getFuncInfo j
let A = setvar<int> "A"

A |+| Set.Empty


let dice = finite_seq [1..6]

4 |?| dice
//let ss = setvar<int> "ss"

//ss |+| Set.Empty