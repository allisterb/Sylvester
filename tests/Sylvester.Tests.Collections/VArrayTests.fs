namespace Sylvester.Tests.Collections

module VArrayTests =

    open Xunit

    open Sylvester.Arithmetic
    open Sylvester.Arithmetic.N10
    open Sylvester.Collections

    [<Fact>]
    let ``Can create VArray``() =
        let arr5 = Array<N<5>, int>(3)
        Assert.NotNull(arr5)
        Assert.Equal(3, arr5.[zero])
        ()
        //let v = VArray(two * hundred + three * ten + four, 0)
        //Assert.IsType<N3<_2, _3, _4>>(v.Length) |> ignore

        

       

