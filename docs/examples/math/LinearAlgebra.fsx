#load "Include.fsx"

open Sylvester
open Dimension
open Matrix

let a, b = realvar "a", realvar "b"

let A = set_pred<real> <@ fun y -> y > 5. @>

4. |?| A

let M = mat_l ``2`` ``2`` <@[%a; 2.; %b; 1]@>

let S = mat_l ``3`` ``3`` <@[ %a; 1; 2; %b; 4; 5. + %b; 6; 7; %a - 4. ]@>

