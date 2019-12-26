namespace Sylvester.Tests.tf

open System

open Xunit

open Sylvester
open Sylvester.Arithmetic.Base10
open Sylvester.tf
open Sylvester.Tests

open TensorFlow

type GraphTests() =
    inherit BaseTest()

    [<Fact>]
    let ``Can create graph`` () =
        let g = Graph<_3, _1>()
        Assert.True(g.Initialized)
        Assert.NotNull(g._Graph)
        Assert.Equal(g.NumInputs.IntVal, 3)
        ()
