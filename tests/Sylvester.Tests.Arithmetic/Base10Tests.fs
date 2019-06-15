namespace Sylvester.Tests.Arithmetic.FixedPoint

module Base10Tests =

    open Xunit
    open Sylvester.Arithmetic

    [<Fact>]
    let ``Can get Base10 digits for integer``() =
        let g = getIntBase10TypeArray(400, 10)
        Assert.Equal(10, g.Length)
        Assert.Equal("4", g.[7].Name)
        Assert.Equal("0", g.[0].Name)


