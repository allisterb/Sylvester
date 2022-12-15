#load "Include.fsx"

open Sylvester
open Arithmetic
open Dimension
open Matrix

let ee = Scalar6 4

let rr = ee + 5
rr.Expr

let a, b = realvar "a", realvar "b"

let c = 1N + 5N

let A = set_pred<real> <@ fun y -> y > 5. @>

4. |?| A

let M = mat ``2`` ``2`` <@[%a; 2; %b; 1]@>

let S = mat ``3`` ``3`` <@[ %a; 1; 2; %b; 4; 5. + %b; 6; 7; %a - 4. ]@>

marray' S
//mcdel ``1`` S

