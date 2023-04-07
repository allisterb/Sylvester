#load "Include.fsx"

open Sylvester
open Sylvester.CAS

// Init Maxima CAS
do Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"

let x = var6<real> "x"
let y = var6<real> "y"

let f = realfun <@fun x -> (2.*x**3. + 1.) @>

//f.Body
//get_vars f.Body
///recombine_func f.Vars (Ops.Diff 1 x.Expr f.Body)
//let f' = diff x f

let rr = diff x x

integrate x f