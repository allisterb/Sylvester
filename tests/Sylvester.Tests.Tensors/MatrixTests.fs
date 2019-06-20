namespace Sylvester.Tests.Tensors

module MatrixTests =

    open Xunit
    open Sylvester.Arithmetic
    open Sylvester.Arithmetic.N10
    open Sylvester.Arithmetic.Collections
    open Sylvester.Tensors

    [<Fact>]
    let ``Can get Base10 digits for integer``() = 
        let d = VArray<N<100>, int>()
        ()