namespace Sylvester.Tests.tf

open System

open Xunit

open Sylvester
open Sylvester.Arithmetic
open Sylvester.Collections
open Sylvester.tf
open Sylvester.Tests

open TensorFlow

type ArithmeticTests() =
    inherit BaseTest()

    [<Fact>]
    let ``Can add scalar`` () =
        let G = setDefaultGraph (TensorGraph<n<5>, n<1>> "G")
        let s0 = Scalar<float32>("s0")
        let s1 = Scalar<float32>("33")
        let r = s0 + s1
        let m1 = Mat<dim<2>, dim<4>>("m1")
        //let j = r + m1
        ()
