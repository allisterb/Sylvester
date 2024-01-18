#load "Include.fsx"

open Sylvester
open Sylvester.CAS
open FSharp.Quotations
// Init Maxima CAS
//do Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"

let x = realvar "x"
let y = realvar "y"
let A = realconst "A"


sprinte <@ real <| 1 + 2 @>

let a = realseq "S" "n" <@ fun n -> 1 + 1 / n |> real @> 

a