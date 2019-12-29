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

type GraphTests() =
    inherit BaseTest()

    [<Fact>]
    let ``Can create graph`` () =
        let g = TensorGraph<two, one>()
        checklt(zero, g.NumInputs)
        Assert.True(g.Initialized)
        Assert.NotNull(g._Graph)
        Assert.Equal((int) g.NumInputs, 7)
        ()

    [<Fact>]
    let ``Can create graph inputs`` () =
        let g = TensorGraph<three, one>()
        checkgt(four, g.NumInputs)
        Assert.True(g.Initialized)
        Assert.NotNull(g._Graph)
        Assert.Equal((int) g.NumInputs, 3)
        let foo = g.ScalarInput<int>("foo")
        g.Inputs <- varray three [|foo; foo; foo|]
        Assert.Equal("foo", g.Inputs.[zero].Name)
        ()
        
