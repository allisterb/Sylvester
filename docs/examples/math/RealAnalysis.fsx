#load "Include.fsx"

open Sylvester
open Sylvester.CAS
open FSharp.Quotations
// Init Maxima CAS
//do Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"

let x = realvar "x"
let y = realvar "y"
let A = realconst "A"

//let f = realfun <@fun x -> (2.*x**3. + a) @>

//fexpr f

let b = x + A + 2
let v = List.head <| get_vars b.Expr
let ff = recombine_func_as<real->real> [v] (b.Expr)
ff


let f = realfun (x + A + 2)

f.[0.]
//f.Body
//get_vars f.Body
///recombine_func f.Vars (Ops.Diff 1 x.Expr f.Body)
//let f' = diff x f

//let rr = diff x x

//integrate x f