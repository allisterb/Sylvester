#load "Include.fsx"

open System.Reflection
open Sylvester
open Sylvester.CAS
open FSharp.Quotations
open Economics
open RealNumbers
open MathNet.Symbolics
fsi.PrintWidth <- 500


Rational(0.3333333333333)
//Infix.parseList("[4,5,6]")
do Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"


//em.GetVar "foo"
//let m = econ_model<ProfitMaximization>(

let x = realvar "x"
let y = realvar "y"
let z = realvar "z"
let f = realfun "f" (x *** 3 + 1)

factor (x***2 + x***4) |> latex
partfrac_of x (4 / ((x+2)*(x*6))) |> latex



//(f.[a] + 1 + a + 6) |> fix {|a=6.|}


solve defaults [x;y;z] [
    x - 6 *x + y == 30.
    x - y == 5.
    z == 5 * y + 2 * x
]

//Maxima.last_output 10
//f.SymbolicFn.ScalarExpr.Expr

//let cp = econ_model<ConsumerPreference>()
//cp.U