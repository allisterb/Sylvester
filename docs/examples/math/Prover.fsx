#load "Include.fsx"

open Sylvester
open RealNumbers


let r, s = realvar2 "r" "s"

real_numbers |- (r + s == s + r)
// Theorem can't be constructed from incomplete proof
theorem real_numbers (2 * (r + s) + 3 * s == (2 * r + 5 * s)) [
    apply_left distrib_mul_add
]