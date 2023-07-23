#load "Include.fsx"

open Sylvester
open Sylvester.CAS
open FSharp.Quotations
// Init Maxima CAS
//do Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"

let x = realvar "x"
let y = realvar "y"

let a = Unchecked.defaultof<real>

let f = realfun <@fun x -> (2.*x**3. + a) @>

fexpr f

let vv = Expr.ValueWithName(0., "d") |> expand_as<real>
vv
let vvv = Scalar<real> vv
let ff = x + 2 - vvv
get_vars ff.Expr
//f.Body
//get_vars f.Body
///recombine_func f.Vars (Ops.Diff 1 x.Expr f.Body)
//let f' = diff x f

//let rr = diff x x

//integrate x f