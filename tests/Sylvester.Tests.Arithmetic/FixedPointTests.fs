namespace Sylvester.Tests

open System
open Xunit

open Sylvester

open Sylvester.Base10Digits
open Sylvester.Base10

module FixedPointTests = 

    [<Fact>]
    let ``Can construct fixed-point numbers``() = 
        let n11056_1 = N5(d1, d1, d0, d5, d6)
        let n11056_b = N5<_1, _1, _0, _5, _6>()
        let (t_5, t_4, t_3, t_2, t_1) = n11056_1.Digits
        let (b_5, b_4, b_3, b_2, b_1) = n11056_b.Digits
        Assert.IsType<_1>(t_5) |> ignore
        Assert.IsType<_6>(t_1) |> ignore
        Assert.True(AreEqual(t_5, t_4))
        Assert.True(IsEqualTo<_0>(t_3))

        Assert.IsType<_1>(b_5) |> ignore
        Assert.IsType<_6>(b_1) |> ignore
        Assert.True(AreEqual(b_5, t_4))
        Assert.True(IsEqualTo<_0>(b_3))

    [<Fact>]
    let ``Can add fixed-point numbers`` () =
        let a = N5(d0, d0, d0, d1, d1)
        let b = N5(d0, d0, d0, d1, d0)
        let g = N5<_0, _5, _0, _2, _1>()
        Assert.IsType<N5<_0, _0, _0, _2, _1>>(a + b) |> ignore
        Assert.IsType<N5<_0, _0, _0, _2, _2>>(a + b + One) |> ignore
        Assert.IsType<N5<_0, _5, _0, _4, _3>>(a + b + One + g) |> ignore

