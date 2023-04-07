namespace Sylvester

open FSharp.Quotations

type RealFun = Function<real, real>

[<AutoOpen>]
module RealFun =
    let realfun f = RealFun(Field.R, Field.R, f)