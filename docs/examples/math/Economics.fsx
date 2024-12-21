#load "Include.fsx"

open System.Reflection
open Sylvester
open Sylvester.CAS
open FSharp.Quotations
open Economics
open RealNumbers
open MathNet.Symbolics
fsi.PrintWidth <- 500

Infix.parseList("[4,5,6]")
//do Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"


//em.GetVar "foo"
//let m = econ_model<ProfitMaximization>(

//let x = realvar "x"
//let f = realfun "f" (x *** 3 + 1)




//f.SymbolicFn.ScalarExpr.Expr

//let cp = econ_model<ConsumerPreference>()
//cp.U