#load "Include.fsx"

open Sylvester
open FSharp.Quotations
let dice = Seq [1..6]

//let outcomes = dice * dice

//let s = outcomes |> ProbabilitySpace

//let P = s.Prob
//let A = Seq [for i in 1..6 -> (1, i)] 
//let B = Seq [for i in 1..6 -> (2, i) ]

//let C = Seq [(9, 9)]
//P  <| (B |+| A)

//let e = prop "Socrates is a man" |> set_prop_true


let x,i,j,k = var4<int>
[<Formula>]
let forall'<'t, 'u> (bound:'u) (range:bool) (body:'t) = body

//<@ (forall (i, j) (i > 0) (2 * i - j < 3 + k)) == false @> 

//g |> expand
//[Formula]
//let suma<'t, 'u> (bound:'u) (range:bool) (body:'t) = Unchecked.defaultof<'t>

//<@ suma @> |> expand

//let v1 = new FSharp.Quotations.Var("k", typeof<int>)
//let v2 = new FSharp.Quotations.Var("j", typeof<int>)
//Patterns.occurs_free [v1;v2] g

[<Formula>]
let s = sum (x) (x = 3) (x * x)
Theory.S |- <@ !s = 9 @>