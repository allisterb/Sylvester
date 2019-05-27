namespace Sylvester.Tests

open System
open Xunit

open Sylvester

module FixedPointTests = 
    
    let r (a, b)  =
        check (a +> b)
        let z = a + b
        checkres(z, z +< ten)
    
    let q =  r (six + one, two)
        
    [<Fact>]
    let ``Can construct fixed-point numbers``() = 
        let n11056_1 = _N5(d1, d1, d0, d5, d6)
        let n11056_b = N9<_0, _0, _0, _0, _1, _1, _0, _5, _6>()
        let (_, _, _, _, t_5, t_4, t_3, t_2, t_1) = n11056_1.Digits
        let (_, _, _, _, b_5, b_4, b_3, b_2, b_1) = n11056_b.Digits
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
        let a = _N5(d0, d0, d0, d1, d1)
        let b = _N5(d0, d0, d0, d1, d0)
        let g = N5<_0, _5, _0, _2, _1>()
        Assert.IsType<N5<_0, _0, _0, _2, _1>>(a + b) |> ignore
        Assert.IsType<N5<_0, _0, _0, _2, _2>>(a + b + one) |> ignore
        Assert.IsType<N5<_0, _5, _0, _4, _3>>(a + b + one + g) |> ignore

    [<Fact>]
    let ``Can compare fixed-point numbers`` () =
        let a = _N5(d3, d0, d0, d8, d2)
        let b = _N5(d4, d9, d0, d2, d2)
        Assert.IsType<True>(a +== a) |> ignore
        Assert.IsType<False>(a +== b) |> ignore
        Assert.IsType<False>(a +!= a) |> ignore
        Assert.IsType<True>(b +> a) |> ignore
        Assert.IsType<False>(a +> b) |> ignore
        Assert.IsType<True>(a +< b) |> ignore

        let g = isZero four.Digits
        
        let testWhile = six -|> While(Not <-< IsZero, Pred)  //While(Not <-< IsZero, Pred <-< Fst)
        ()
        //()
        //Assert.IsType<N0> testWhile

        
 (*let r = three * ten + two
        let h = !! (a +== b)
        let i = a +== zero
        let j = (zero + one) +== one*)




