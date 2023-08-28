#nowarn "3391"
#load "Include.fsx"

open Sylvester
open Patterns
open FSharp.Quotations
open PropCalculus
open Integers
open RealNumbers


let r, s = realvar2 "r" "s"

real_numbers |- (r + s == s + r)
// Theorem can't be constructed from incomplete proof
proof real_numbers (2 * (r + s) + 3 * s == (2 * r + 5 * s)) [
    apply_left distrib_mul_add
]