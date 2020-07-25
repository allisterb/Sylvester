namespace Sylvester.Tests

module LinearAlgebra = 

    open System
    open Xunit

    open Sylvester
    open Sylvester.Arithmetic

    [<Fact>]
    let ``Can construct matrices`` () =
        let m1 = Mat<one, three>([4.;5.;6.])
        Assert.NotNull m1
        ()
