namespace Sylvester.Tests.Packages

module tfTests = 

    open System
    open Xunit

    open Sylvester.Arithmetic
    open Sylvester.tf

    [<Fact>]
    let ``Can create TensorGraph`` () =
        defaultGraph <- Graph<n<4>, n<1>>()
       

        //let m0 = Matrix<dim<4>, dim<3>, int>("x") // Creates a TensorFlow placeholder node and output edge with shape 4x3 and name m_0.

        //Assert.NotNull(m0)
