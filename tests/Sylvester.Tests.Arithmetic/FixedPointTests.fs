namespace Sylvester.Tests

open System
open Xunit

open Sylvester

open Sylvester.Base10Digits

module FixedPointTests = 

    [<Fact>]
    let ``Can add fixed-point numbers`` () =
        let a = N5(d0, d0, d0, d1, d1)
        let b = N5(d0, d0, d0, d1, d0)
        Assert.IsType<N5<_0, _0, _0, _2, _1>>(a + b) |> ignore
        Assert.IsType<N5<_0, _0, _0, _2, _2>>(a + b + One) |> ignore


