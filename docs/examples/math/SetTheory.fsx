#load "Include.fsx"

open FSharp.Quotations

open Sylvester
let j:Expr<Set<int>> = <@ Set.Empty |+| Set.Empty @>

getFuncInfo j
let A = SetTerm<int> <@ Set.Empty@>

A + Set.Empty
