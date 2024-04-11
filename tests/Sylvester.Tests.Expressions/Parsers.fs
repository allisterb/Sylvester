namespace Sylvester.Tests.Expressions

module Parsers = 

    open Xunit

    open Sylvester
    open MathNet.Symbolics

    [<Fact>]
     let ``Can parse function def``() =
        let R = Infix.parse("sin(x)")
        Assert.Equal (R, Infix.parse("BAR()"))