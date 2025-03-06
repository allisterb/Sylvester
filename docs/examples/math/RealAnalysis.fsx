#load "Include.fsx"

open Sylvester
open RealNumbers


do CAS.Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"

let x = realvar "x"
let y = realvar "y"
let A = realconst "A"

lim x inf (3 + 1 / x - 1 / 2 *** x)

lim x neginf (sqrt((4 - 1 / x) / ( 1 - 4 / x *** 3)))

lim x inf (1 / (x^^3))