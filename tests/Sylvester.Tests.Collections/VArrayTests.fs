namespace Sylvester.Tests.Collections

module VArrayTests =

    open Xunit
    open Sylvester.Arithmetic
    open Sylvester.Arithmetic.N10
    open Sylvester.Collections

    [<Fact>]
    let ``Can create VArray``() =
        let t = typeof<VArray<_,_,_,_,_,_,_,_,_,_>>
        let c = t.GetMethod("create")
        ()
        //let v = VArray(two * hundred + three * ten + four, 0)
        //Assert.IsType<N3<_2, _3, _4>>(v.Length) |> ignore

        

       

