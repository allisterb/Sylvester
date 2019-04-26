namespace Sylvester.Tests

open System
open Xunit

open Sylvester.Base10
open Sylvester.Base10Digits

module FixedPointTests = 

    [<Fact>]
    let ``Can add fixed-point number`` () =
        let a = N5(d0, d0, d0, d1, d1)
        let b = N5(d0, d0, d0, d1, d0)
        Assert.IsType(typedefof<N5<_0, _0, _0, _2, _1>>, a + b)
        Assert.IsType(typedefof<N5<_0, _0, _0, _0, _1>>, a - b)


