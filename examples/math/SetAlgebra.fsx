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


let x,i,j,k = var4<int>
//[<Formula>]
//let forall'<'t, 'u> (bound:'u) (range:bool) (body:'t) = body

[<Formula>]
let g = forall (i, j) (i > 0) (2 * i - j < 3 + k) 

//<@ g @> |> expand

//let suma<'t, 'u> (bound:'u) (range:bool) (body:'t) = Unchecked.defaultof<'t>

//<@ suma @> |> expand

let v0 = new FSharp.Quotations.Var("i", typeof<int>)
let v1 = new FSharp.Quotations.Var("k", typeof<int>)
let v2 = new FSharp.Quotations.Var("j", typeof<int>)
let v3 = new FSharp.Quotations.Var("x", typeof<int>)
let v4 = new FSharp.Quotations.Var("k", typeof<int>)
//Patterns.occurs_free [v1;v2] <@ g @>

//<@ g @> |> expand |> Patterns.bound_vars
//<@ e @> |> expand

open FSharp.Quotations
open Patterns
let (|OnePoint|_|) =
    function
    | Equals(Quantifier(_,bound, Equals(Var x, e), P1), P2) when vequal_single x bound && sequal P2 (subst_var_value x e P1) -> Some bound
    //| Quantifier(_,bound,range,body) -> Some bound
    | _ -> None

let z  = 
    function
    | OnePoint x -> Some true
    | _ -> Some false
let s4 = <@ sum x (x = 3) (x * x) = (3 * 4) @> |> expand 
s4.Substitute(fun v -> Some <@@ 3 * 3@@>)
//let s5 = subst_var_value v3 <@ 3 * 3 @> s4

//s4
//src s
//z s4

//Theory.S |- s4
[<Formula>]
let d1 = forall (i,j) (j > 0) (i > j)
[<Formula>]
let d2 = forall (i,j) (j > 0) (i > j + 10)


Theory.S |- <@ d1 |&| d2 = forall (i, j) (j > 0) (d1 |&| d2) @>