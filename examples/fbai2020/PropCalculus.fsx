#load "Include.fsx"

open Sylvester
open Sylvester.Nymph

let m = Parser.parse<bool> "p = q = (q = p)"

