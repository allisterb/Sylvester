#load "Include.fsx"

open Sylvester
open Sylvester.Nymph


let m = Parser.parse<int64> "p = q = (q = p)"

