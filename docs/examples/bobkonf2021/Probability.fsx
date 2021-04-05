#load "IncludeMath.fsx"

open FSharp.Quotations
open System

open Sylvester
open PropCalculus

[<CustomEquality; NoComparison>]
type Sym<'t> = Sym of Expr<'t> with
     member x.Expr = let (Sym e) = x in e
     member x.Item(i:int) = x
     interface IEquatable<Sym<'t>> with member a.Equals b = a.Expr.ToString() = b.Expr.ToString()
     override a.GetHashCode() = (a.Expr.ToString()).GetHashCode()
     override a.Equals (_b:obj) = 
             match _b with 
             | :? Sym<'t> as e -> (a :> IEquatable<Sym<'t>>).Equals e
             | _ -> false
     override x.ToString() = src (x.Expr)
     static member (+)(l:Sym<'t>, r:Sym<'t>) = formula<Sym<'t>>
     static member (*)(l:Sym<'t>, r:Sym<'t>) = formula<Sym<'t>>
     static member (..)(l:Sym<'t>, r:Sym<'t>) = formula<Sym<'t>>
     static member (..+)(l:Sym<'t>, r:Sym<'t>) = formula<Sym<'t>>
     static member (..*)(l:Sym<'t>, r:Sym<'t>) = formula<Sym<'t>>

let elem<'t> n = 
    let v = Expr.Var(Var(n, typeof<'t>)) in <@ %%v:'t @> |> Sym
let a = elem<int> "a"
let i = var<int>
a.[i]

a.[0]*a.[1]..*a

let ff = 4::[]
let x = var<int>
let rr = pred<int>
let zz = proof prop_calculus <@ x |?| set rr x = forall x (x > 0) (rr x = rr x)@> []

[<Formula>]
let rec f =
    function
    | n when n < 6 -> n + 3
    | n -> f(n - 1)

//let iff = infinite_seq f
<@ infinite_seq f @>

let urn = sseq [1..5] * sseq [1..4]



let S = prob_space urn
let P = prob_measure S

[<Formula>]
let E1 = urn |>| (fun s -> fst s = 5)

let E2 = urn |>| (fun s -> snd s < 4)
let E3 = urn |>| (fun s -> fst s + snd s >= 8)


P(E1 |/| S ) + P(E1)

let ee = var<bool>

expand <@ urn @>
