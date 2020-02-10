#load "MathInclude.fsx"

open System.Linq

open Sylvester

let dice = seq {1..6} |> Seq
dice.Length

let s = Subsets(dice, Seq [Empty; Seq [2; 3]; Seq [4; 5]; dice]) 
let g = SigmaAlgebra(s)

g.AsLattice.Least

let x = dice.Subsets(fun s -> s = Empty || s.Length = 6 || s = dice)
x.Count()

//let dd = (dice * dice).Subset(fun (d1, d2) -> d1 + d2 = 5)

let zz = dice.Rel(fun (a, b) -> b = a * 2)

//let zzz = Nz.Set.Rel(fun (a, b) -> b = a * 2)

//let hh = Nz.Set.Rel(fun(a,b) -> b = 2 * a)

//hh |> Seq.item 8

let gn = Seq [|5;6;6;7;8;9|] 
(gn * gn).Length