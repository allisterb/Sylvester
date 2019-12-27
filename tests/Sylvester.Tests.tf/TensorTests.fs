namespace Sylvester.Tests.tf

open System

open Xunit

open Sylvester
open Sylvester.Arithmetic.Base10
open Sylvester.tf
open Sylvester.Tests

open TensorFlow

type TensorTests() =
    inherit BaseTest()

    [<Fact>]
    let ``Can create new empty tensor``() =
        let t = new Tensor<int64>(2, 3, 5)
        Assert.True(t.Initialized);
        Assert.Equal(0L, t.[0])
        t.[1] <- 66L
        Assert.Equal(66L, t.[1])
        ()

    [<Fact>]
    let ``Can create new tensor from array``() =
        let arr = Array4D.create (22) (5) (6) (7) 8.0f
        let t = new Tensor<float32>(arr)
        Assert.True(t.Initialized);
        tf_tensor.TF_DeleteTensor(t._Tensor)
        let d2 = Array3D.create (700) (1) (44) 7.32
        let t2 = new Tensor<float>(d2)
        Assert.True(t2.Initialized);
        Assert.Equal(8.0f, t.[0]); 
        Assert.Equal(700 * 44 * 8, int t2.Length)
        //tf_tensor.TF_DeleteTensor(t2._Tensor);
        //Assert.False(t2.Initialized)
        ()
        
