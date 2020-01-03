namespace Sylvester.Tests.tf

open System
open System.Linq

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
        let G = TensorGraph<n<5>, n<1>> "G"
        let s0 = Scalar<float32>("s0")
        let s1 = Scalar<float32>("33")
        let r = s0 + s1
        Assert.Equal(3, G.Nodes.Count)
        Assert.Equal("G/Add_0", G.Nodes.Keys.Last())
        Assert.Equal(2, G.Nodes.Last().Value.Inputs.Count())
        Assert.Equal(s0 :> Edge, G.Nodes.Values.Last().Inputs.[0])
        Assert.Equal(s1 :> Edge, G.Nodes.Values.Last().Inputs.[1])
