namespace Sylvester

open FSharp.Quotations

type RealFun(f) = 
    inherit Function<real, real, Scalar<real>>(Field.R, Field.R, f, Scalar<real>)
    
[<AutoOpen>]
module RealFun =
    let realfun f = RealFun f