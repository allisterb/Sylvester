namespace Sylvester.Tests.tf

open System

open Xunit

open Sylvester
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.tf
open Sylvester.Tests

open TensorFlow

type GraphTests() =
    inherit BaseTest()

    [<Fact>]
    let ``Can create graph`` () =
        let g = TensorGraph<three, one>()
        Assert.True(g.Initialized)
        Assert.NotNull(g._Graph)
        Assert.Equal((int) g.NumInputs, 3)
        ()
        
