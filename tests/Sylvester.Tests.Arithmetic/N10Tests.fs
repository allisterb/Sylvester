namespace Sylvester.Tests.Arithmetic.FixedPoint

module N10 = 

    open System
    open System.Reflection
    open Xunit
    open Sylvester.Arithmetic
    open Sylvester.Arithmetic.N10
       
    [<Fact>]
    let ``Can construct N10 fixed-point number``() = 
        let n11056_1 = _N5(d1, d1, d0, d5, d6)
        let n11056_b = N10<_0, _0, _0, _0, _0, _1, _1, _0, _5, _6>()
        let (_, _, _, _, _, t_5, t_4, t_3, t_2, t_1) = n11056_1.Digits
        let (_, _, _, _, _, b_5, b_4, b_3, b_2, b_1) = n11056_b.Digits
        Assert.IsType<_1>(t_5) |> ignore
        Assert.IsType<_6>(t_1) |> ignore
        Assert.True(areEqual(t_5, t_4))
        Assert.True(isEqualTo<_0>(t_3))

        Assert.IsType<_1>(b_5) |> ignore
        Assert.IsType<_6>(b_1) |> ignore
        Assert.True(areEqual(b_5, t_4))
        Assert.True(isEqualTo<_0>(b_3))

    [<Fact>]
    let ``Can construct N10 fixed-point number via reflection``() = 
        let _n = 466710
        let t = getIntBase10TypeArray(_n, 10)
        Assert.Equal(10, t.Length)
        let n = typedefof<N10<_,_,_,_,_,_,_,_,_,_>>.MakeGenericType(getIntBase10TypeArray(_n, 10))
        ()


    [<Fact>]
    let ``Can add N10 fixed-point numbers`` () =
        let a = _N5(d0, d0, d0, d1, d1)
        let b = _N5(d0, d0, d0, d1, d0)
        let g = N5<_0, _5, _0, _2, _1>()
        Assert.IsType<N5<_0, _0, _0, _2, _1>>(a + b) |> ignore
        Assert.IsType<N5<_0, _0, _0, _2, _2>>(a + b + one) |> ignore
        Assert.IsType<N5<_0, _5, _0, _4, _3>>(a + b + one + g) |> ignore

    [<Fact>]
    let ``Can compare N10 fixed-point numbers`` () =
        let a = _N5(d3, d0, d0, d8, d2)
        let b = _N5(d4, d9, d0, d2, d2)
        Assert.IsType<True>(a +== a) |> ignore
        Assert.IsType<False>(a +== b) |> ignore
        Assert.IsType<False>(a +!= a) |> ignore
        Assert.IsType<True>(b +> a) |> ignore
        Assert.IsType<False>(a +> b) |> ignore
        Assert.IsType<True>(a +< b) |> ignore

    [<Fact>]
    let ``Can constrain N10 fixed-point numbers`` () =
        check(one + two +< ten)
        check((ten + two) +== (six * two))

    [<Fact>]
    let ``Can overflow N10 fixed-point number`` () =
        let a = N10(d9, d9, d8, d7, d6, d5, d4, d3, d2, d1)
        let b = N10(d9, d9, d8, d7, d6, d5, d4, d3, d2, d1)
       
        Assert.IsType<N10Overflow>(a + b) |> ignore
        Assert.IsType<N10Overflow>(a + a) |> ignore
        Assert.IsType<N10Underflow>(a - (b + one)) |> ignore
        Assert.IsType<N10Underflow>(zero - (b + one)) |> ignore
        Assert.IsType<N10Underflow>(b - (b + one)) |> ignore

    [<Fact>]
    let ``Can convert N10 fixed-point number``() =
        let a = N10(d9, d9, d8, d7, d6, d5, d4, d3, d2, d1)
        let b = N10(d0, d9, d8, d7, d6, d5, d4, d3, d2, d1)
        Assert.Equal (987654321, (int) b)
        Assert.Equal (987654321ul, (uint32) b)
        Assert.Equal (9987654321UL, (uint64) a)
        Assert.Equal (987654921, b + 600)
        Assert.Equal (9987654331UL, a + 10UL)
      


        
       

       




