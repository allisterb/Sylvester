#load "Include.fsx"

open Sylvester
open Arithmetic
open Dimension
open Matrix

let ee = Scalar6 4

let r = ee - 5

let v = Scalar6.var6<real> "v"

(4Q + v + 2)