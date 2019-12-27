namespace Sylvester.Tests.tf

open System

open Xunit

open Sylvester
open Sylvester.Arithmetic.Base10
open Sylvester.tf
open Sylvester.Tests

open Sylvester.tf.TensorUtil

open TensorFlow

type TensorTests() =
    inherit BaseTest()

    [<Fact>]
    let ``Can get total length of array`` () =
        let arr = Array2D.create 50 5 1
        Assert.Equal(250, arrayTotalLength arr)
        let arr3 = Array3D.create 16 5 7 4
        Assert.Equal(560, arrayTotalLength arr3)
        
