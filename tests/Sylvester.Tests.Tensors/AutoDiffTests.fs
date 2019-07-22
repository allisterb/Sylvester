namespace Sylvester.Tests.Tensors

module AutoDiffTests = 
    
    open Xunit
    open DiffSharp.AD.Float64

    open Sylvester.Arithmetic
    open Sylvester.Arithmetic.N10
    open Sylvester.Tensors
    open Sylvester.Tensors.AutoDiff

    [<Fact>]
    let ``Can autodiff scalar function``() = 
        let a = scalar 0.         
        
        let f x =  sin x

        let dz = (diff f) <| Scalar.D a
        Assert.NotNull(dz)
        Assert.Equal(1.0, (float) dz)

    [<Fact>]
    let ``Can autodiff vector-scalar function``() =
        let v = vnew five 3.0
        let f (v:DV) = sin v.[0] + exp v.[1] 
        let df = grad f
        let dz = df(Vector.D(v))
        Assert.NotNull(dz)



  
