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


        let m33one = mnew three three 1.0
        let m33two = mnew three three 2.0
        let m33three = m33one + m33two

        Assert.Equal(3.0, m33three.[zero, zero])

        let m28 = mnew two eight 5.1f

        let v8 = vnew eight 3.2f
        let v7 = vnew seven  3.2f

        let v2 = m28 * v8
        
        Assert.Equal(2, v2._Array.Length)

        let m83 = mnew eight three 1.0f

        let m74 = mnew seven four 1.0f
        
        let m23 = m28 * m83

        Assert.NotEmpty(m23._Array)
        Assert.IsType<N1<_2>>(m23.Dim0) |> ignore
        Assert.Equal(m23._Array.Length, 6)

        let m83' = m83 * scalar 2.0f

        Assert.Equal(m83.[five, zero] * 2.0f, m83'.[five, two])
        
        



        
    