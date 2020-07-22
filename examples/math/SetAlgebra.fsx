#load "Include.fsx"

open Sylvester
open SetAlgebra
open FSharp.Quotations
let n,t = var2<int>
let A,B = var2<Set<obj>>

//SetComprehension(5 * 3, (fun sc x -> x % 3 = 0))
let X = Seq [for i in 0..10 -> i]
//X.Body t

let X2 = infiniteSeq (fun n -> n + 5)
//X2.Range t

let Y = SetComprehension(t > 5, t * 3, (fun sc x -> x % 3 = 0)) |> Set
//Y.Range n
//<@ forall n (%%Y.Range n) (n = %%Y.Body n) @> |> expand

let Y' = Y.Subset(fun a -> a > 20)

let G = SetGenerator(Seq.initInfinite(fun g -> g + 5), (fun x -> true)) |> Set.fromGen
G.Body.ToString()