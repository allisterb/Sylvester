#load "Include.fsx"

open Sylvester
open Sylvester.CAS

open Economics
open RealNumbers

fsi.PrintWidth <- 500


do Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"

let g = gamble [1000.,0.25; 100.,0.75]

expectation g