#load "Include.fsx"

open Sylvester
open Arithmetic
open Dimension
open Vector

let a = Term.var6<real> "a"

let M = vec ``2`` [a + 2; 2]

let m = M * M

