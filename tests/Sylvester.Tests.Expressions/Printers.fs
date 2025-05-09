﻿namespace Sylvester.Tests.Expressions

module Printers = 

    open Xunit

    open Sylvester
 
    [<Fact>]
    let ``Can print trigonometric functions``() =
        Assert.Equal ((4Q * 6), 24Q)
        Assert.Equal ((6 * 4Q), 24Q)
        Assert.Equal (24Q, (4Q * 6))

    [<Fact>]
    let ``Can divide integer and rational``() =
        Assert.Equal ((31Q / 2), Rational(31, 2))
        //Assert.Equal (24Q, (4Q * 6))