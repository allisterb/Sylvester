#load "Include.fsx"

open Sylvester
open SetAlgebra
open FSharp.Quotations
let n,t = var2<int>
let A,B = var2<Set<obj>>
let se = var<seq<int>>

Z |> Seq.take 4 |> Seq.toArray


[<Formula>]
let Y = SetComprehension(t >= 0, t * 3) |> Set

let Zz = Y.Subset(fun a -> a > 0) 

Zz.Body n
//Display.print_formula <@ forall n (%%Y.Range n) (n = (%%Y.Body n)) @>
proof set_algebra <@ forall n (%%Y.Range n) (n = (%%Y.Body n)) @> []

//[<Formula>]
//let ST = SetComprehension(t > 5, Seq.initInfinite(fun n -> n + 10), set_no_test) |> Set
//ST.
//[<Formula>]
//let SSS = Seq.initInfinite(fun n -> n + 10) |> Seq
//SSS.Body n