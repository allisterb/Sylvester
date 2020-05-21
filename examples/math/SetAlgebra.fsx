#load "Include.fsx"

open System.Linq
open Sylvester
open SetAlgebra

let dice = seq {1..6} |> Seq

let outcomes = dice * dice

let s = outcomes |> SigmaAlgebra
