namespace Sylvester.Tests.Collections

module VArrayTests =

    open Xunit

    open Sylvester
    open Arithmetic 
    open N10
    open Sylvester.Collections

    [<Fact>]
    let ``Can create VArray``() =
        let zero  = new dim<0>()
        let arr5 = Array<dim<5>, int>(3)
        Assert.NotNull(arr5)
        Assert.Equal(3, arr5.[zero])
        ()
        //let v = VArray(two * hundred + three * ten + four, 0)
        //Assert.IsType<N3<_2, _3, _4>>(v.Length) |> ignore
    let ``Can create non-empty array``() =
        let arr = Array<``1``, string>(array.Empty<string>())
        Assert.NotNull arr
       

