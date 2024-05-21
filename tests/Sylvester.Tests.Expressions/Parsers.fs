namespace Sylvester.Tests.Expressions

module Parsers = 

    open Xunit

    open Sylvester
    open MathNet.Symbolics

    [<Fact>]
     let ``Can parse function def``() =
        let R = Infix.parse("BAR(x) + 1")
        Assert.Equal (R, Infix.parse("BAR()"))