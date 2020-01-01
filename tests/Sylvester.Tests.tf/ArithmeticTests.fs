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
        setDefaultGraph (TensorGraph<n<5>, n<1>> "G")
        let s0 = new Scalar<float32>("s0")
        let s1 = new Scalar<float32>("33")
        let r = s0 + s1
        ()
