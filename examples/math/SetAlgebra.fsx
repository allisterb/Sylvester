open Microsoft.FSharp.Core.Operators
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
[<Formula>]
let e = prop "Socrates is a man" 


let i, j, k = var3<int>
let x, y, z = var3<int>

//[<Formula>]
//let forall'<'t, 'u> (bound:'u) (range:bool) (body:'t) = body

[<Formula>]
//let g = forall (i, j) (i > 0) (2 * i - j < 3 + k) 

//<@ g @> |> expand

//let suma<'t, 'u> (bound:'u) (range:bool) (body:'t) = Unchecked.defaultof<'t>

//<@ suma @> |> expand

let v0 = new FSharp.Quotations.Var("i", typeof<int>)
let v1 = new FSharp.Quotations.Var("k", typeof<int>)
let v2 = new FSharp.Quotations.Var("j", typeof<int>)
let v3 = new FSharp.Quotations.Var("x", typeof<int>)
let v4 = new FSharp.Quotations.Var("k", typeof<int>)

open FSharp.Quotations
open Patterns
//[<Formula>]
let d1 = forall (i,j) (i > j)
[<Formula>]
let d2 = forall (i, j) (i > j + 10)
//Theory.S |- <@ forall (i, j, k) (i > 0 ==> (i + j  > 5)) = (forall i (i > 0 ==> (forall (j, k) (i > 0 ==> (i + j  > 5))))) @>
//Theory.S |- <@ forall (i, j) (i + j  > 5) = forall (x, y) (x + y > 5) @>

Theory.S |- <@ forall (i, j) (forall k (i + j + k > 0)) = (forall k (forall (i,j) (i + j + k < 0))) @>
//proof PropCalculus.prop_calculus <@ d1 |&| d2 = forall (i, j) ((i > j) |&| (i > j + 10)) @> []

//open System.Reflection

//FormulaDisplay.print_formula <@ true |&| false @>
//Formula.FormulaModuleType.GetMember("op_BarAmpBar").[0].

//getFuncName <@ (+) @>
//FormulaDisplay.replace_with_op_unicode_display <@ (==>) @>
////Formula.(|&|)
//FormulaDisplay.print_formula <@ false ||| true @>