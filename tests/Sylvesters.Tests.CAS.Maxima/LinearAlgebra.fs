namespace Sylvester.Tests.CAS

open FSharp.Quotations

open Sylvester

open Matrix
open LinearEquations


module LinearAlgebra = 



do CAS.Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"

fsi.PrintWidth <- 500


MathNet.Symbolics.Infix.parseMatrix "matrix([1,-3,0,-2],[0,1,-1/5,6/5],[0,0,1,-1],[0,0,0,1])"

let l = realvar "l"

let A = sqmat [1;2;-1;4;-1;3;0;2;2;1;1;2;1;4;1;3]

mcharpoly l A

