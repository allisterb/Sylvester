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
        let s = a
        let x = five + six
        let zzzz = minsrow a three (vec five (Array.create 5 0.))
        Assert.NotEmpty(zzzz._Array)

    [<Fact>]
    let ``Matrix operations work``() =
        let vvv = vrand five
        Assert.NotEmpty(vvv._Array)
        let mmm = mnew five five 1.0f
        Assert.NotEmpty(mmm._Array)
        let jjj = mmm +@. vvv
        Assert.NotEmpty(jjj._Array)
        Assert.IsType<N1<_6>>(jjj.Dim0) |> ignore
        Assert.IsType<N1<_5>>(jjj.Dim1) |> ignore

        let jjjj = mmm +@@. vvv

        Assert.NotEmpty(jjjj._Array)
        Assert.IsType<N1<_5>>(jjjj.Dim0) |> ignore
        Assert.IsType<N1<_6>>(jjjj.Dim1) |> ignore

        
    