#load "Include.fsx"

open Sylvester
open Sylvester.CAS
open FSharp.Quotations
open Economics
open RealNumbers

fsi.PrintWidth <- 500

do Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"

let m = econ_model<ProfitMaximization>()

sprints m.F.ScalarExpr