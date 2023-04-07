#load "Include.fsx"

open Sylvester
open Sylvester.CAS

// Init Maxima CAS
do Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"

let x = var6<real> "x"
let y = var6<real> "y"

diff x 

let f = realfun <@fun x -> (2.*x**3. + 1.) @>

let f' = diff_fun x f

f'.[y]

