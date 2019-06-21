namespace Sylvester.Tests.Arithmetic.Collections

module VArrayTests =

    open Xunit
    open Sylvester.Arithmetic
    open Sylvester.Arithmetic.N10
    open Sylvester.Arithmetic.Collections

    [<Fact>]
    let ``Can create VArray``() =
        let v = VArray(two * hundred + three * ten + four, 0)
        Assert.IsType<N3<_2, _3, _4>>(v.Length) |> ignore

        

       

