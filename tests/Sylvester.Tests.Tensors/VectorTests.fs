namespace Sylvester.Tests.Tensors

module VectorTests = 
    
    open Xunit
    open Sylvester.Arithmetic
    open Sylvester.Arithmetic.N10
    open Sylvester.Tensors

    [<Fact>]
    let ``Can create vector``() = 
        let a = vec (five * hundred) (Array.create 500 7.) 
        Assert.IsType<N3<_5, _0, _0>>(a.Dim0) |> ignore
        Assert.Equal(7., a.[ten])
        a.SetVal(two * hundred,  5.)
        Assert.Equal(5., a.[two * hundred])
        let b = a.[two * hundred..three * hundred]
        Assert.Equal(101, b.Dim0.IntVal)
        Assert.Equal(5., b.[zero])

    [<Fact>]
    let ``Vector operations work``() = 
        let a = vec (five) (Array.create 5 7.) 
        Assert.IsType<N1<_5>>(a.Dim0) |> ignore
        Assert.Equal(7., a.[four])
        a.SetVal(two,  5.)
        Assert.Equal(5., a.[two])

        let m = vmax a
        Assert.Equal(7., m.Val)

        let vvv = vrand five
        Assert.NotEmpty(vvv._Array)

        let v10 = vrand ten

        let v15 = vrand (ten * five)

        Assert.NotEqual(0.0f, v10.[zero])
        let s = v10 + v10

        Assert.NotEmpty(s._Array)

        let t = s * scalar 5.0f

        Assert.NotEmpty(t._Array)

        Assert.Equal(10.0f * v10.[zero], t.[zero])

    [<Fact>]
    let ``Can add two vectors``() = 
        let v200 = vrand (two * hundred)
        let v200b = vrand (two * hundred)
        let s = v200 + v200b
        Assert.NotEqual(0.0f, s.[zero])
        Assert.Equal(v200.[four] + v200b.[four], s.[four])

    [<Fact>]
    let ``Can subtract two vectors``() = 
        let v200 = vrand (two * hundred)
        let v200b = vrand (two * hundred)
        let s = v200 + v200b
        Assert.NotEqual(0.0f, s.[zero])
        Assert.Equal(0.0f, v200.[four] - v200.[four])
