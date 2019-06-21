namespace Sylvester.Tests.Tensors

module MatrixTests =

    open Xunit
    open Sylvester.Arithmetic
    open Sylvester.Arithmetic.N10
    open Sylvester.Arithmetic.Collections
    open Sylvester.Tensors

    [<Fact>]
    let ``Can create matrix``() = 
        let a = mat four five (Array2D.create 4 5 1.) 
        Assert.IsType<NFour>(a.Dim0) |> ignore
        Assert.IsType<NFive>(a.Dim1) |> ignore
        Assert.Equal(1., a.[two, two])
        a.SetVal(two, two, 5.)
        Assert.Equal(5., a.[two, two])
        let b = a.[two..three, zero..two]
        Assert.Equal(2, b.Dim0.IntVal)
        Assert.Equal(3, b.Dim1.IntVal)
        Assert.Equal(5., b.[zero, two])

    