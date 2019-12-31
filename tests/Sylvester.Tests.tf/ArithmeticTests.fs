namespace Sylvester.Tests.tf

open System

open Xunit

open Sylvester
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Collections
open Sylvester.tf
open Sylvester.Tests

open TensorFlow

type ArithmeticTests() =
    inherit BaseTest()

    [<Fact>]
    let ``Can add scalar`` () =
        resetDefaultGraph()
        let s0 = new Scalar<float32>()
