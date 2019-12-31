﻿namespace Sylvester.Tests.tf

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
        let g = TensorGraph<seven, one>()
        checklt(zero, g.NumInputs)
        Assert.True(g.Initialized)
        Assert.NotNull(g._Graph)
        Assert.Equal((int) g.NumInputs, 7)
        ()

    [<Fact>]
    let ``Can create graph inputs`` () =
        let g = TensorGraph<dim<3>, one>()
        checkgt(four, g.NumInputs)
        Assert.True(g.Initialized)
        Assert.NotNull(g._Graph)
        Assert.Equal((int) g.NumInputs, 3)
        let foo = g.ScalarInput<int>("foo")
        g.Inputs <- varray (new dim<3>()) [|foo; foo; foo|]
        //vainit [|foo; foo; foo|] g.Inputs
        Assert.Equal("foo", g.Inputs.[zero].Name)
        ()

    [<Fact>]
    let ``Can create default graph inputs`` () =
        let A = new Matrix<five, one, int>("A")
        Assert.Equal("A_0", A.Name)
        Assert.Equal("_", A.Graph.NameScope)
        let B = new Matrix<n<5>, n<10>, int>("B")
        checkgt(seven, B.Dim0)
        let D = Mat<dim<4>, dim<3>>("D")
        let g = TensorGraph<n<5>, n<1>>()
        //let a = g.MatrixInput<n<10>, n<5>, int>("a")
        //Assert.Equal("a_0", a.Name)
        //Assert.Equal(a.Graph.NameScope, g.NameScope)
        ()
        


        