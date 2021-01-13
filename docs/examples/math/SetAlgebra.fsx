#load "Include.fsx"

open Sylvester
open SetAlgebra

let n,t = var2<int>
let A,B = var2<Set<obj>>
let se = var<seq<int>>

Z
Z |> Seq.take 4 |> Seq.toArray


let Y = Set (compr <@ (t >= 0) @> <@ (t * 3) @>)
let Z = Seq [0.; 1.; 2.]
Z.[1]
//Y.Body.Substitute(fun _ -> Some (FSharp.Quotations.Expr.Value(4, typeof<int>)))
Y.[4] 
let Zz = Y.Subset(<@ t < 0 @>) 

let YY = Set.Of(<@ t >= 0 @>, <@ t * 3 @>)

YY.[4]
Zz.Body 
//Display.print_formula <@ forall n (%%Y.Range n) (n = (%%Y.Body n)) @>
proof set_algebra <@ forall n (%%Y.Range n) (n = (%%Y.Body n)) @> []

//[<Formula>]
//let ST = SetComprehension(t > 5, Seq.initInfinite(fun n -> n + 10), set_no_test) |> Set
//ST.
//[<Formula>]
//let SSS = Seq.initInfinite(fun n -> n + 10) |> Seq
//SSS.Body n