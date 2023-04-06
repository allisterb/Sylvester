#load "Include.fsx"

open Sylvester
open Sylvester.CAS

// Init Maxima CAS
do Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"

let x = var6<real> "x"

diff x (2*x**3 + 1)