#load "Include.fsx"

open Sylvester
open MicroEconomics

let P, Q = realvar "P", realvar "Q"
let A = realconst "A"
let Qs = demand (P == 2 * Q + 4 + A)

Qs.[0.]