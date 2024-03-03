#load "Include.fsx"

open Sylvester
open Sylvester.CAS
open FSharp.Quotations
open Economics
open RealNumbers

fsi.PrintWidth <- 500

let var = Var("v", typeof<real>)
Expr.Let(var, <@ 0.0 @>, Expr.Var(var))
do Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"

let g = gamble [1000.,0.25; 100.,0.75]

let fs = realfun_s "F" "v"
fs.ScalarExpr.Expr