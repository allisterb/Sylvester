#load "Include.fsx"

open Sylvester
open RealNumbers


do CAS.Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"

let x = realvar "x"
let y = realvar "y"
let A = realconst "A"

lim x inf (3 + 1 / x - 1 / 2***x)

lim x neginf (sqrt((4 - 1 / x) / ( 1 - 4 / x***3)))

lim x inf (((x***3) - 3 * (x***2) + 2) / (4 * x *** 3 + 6 * x))

lim x inf (sin x / x)

lim x inf ((x***2 + x +  1) / (x + x***2 + x***3))

//lim x inf ((sqrt (x + sin x - 2)) / ((sqrt x) + sin x - 2))

lim x inf (x***3 + x + 2)

lim x inf ((x***3 + 2*x + 2) / (x***2 + 1))