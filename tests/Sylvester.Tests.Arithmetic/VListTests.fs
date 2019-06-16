namespace Sylvester.Tests.Arithmetic.Collections

module VListTests =

    open Xunit
    open Sylvester.Arithmetic
    open Sylvester.Arithmetic.N10
    open Sylvester.Arithmetic.Collections

    [<Fact>]
    let ``Can create VList``() =
        let v = VList<N3<_2, _3,_4>, int>()
        Assert.IsType<N3<_2, _3, _4>>(v.Length) |> ignore

        

       

