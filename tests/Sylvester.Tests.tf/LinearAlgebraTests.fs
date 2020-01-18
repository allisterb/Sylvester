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

type LinearAlgebraTests() =
    inherit BaseTest()

    [<Fact>]
    let ``Can add matrices`` () =
        defaultGraph <- TensorGraph<N<5>, N<1>>()
        Assert.Equal("", defaultGraph.NameScope)
        let msum = 
            use mops = scope "mops"
            Assert.Equal("mops", defaultGraph.NameScope)
            let a = Matrix<dim<33>, dim<5>, int>("a")
            let b = Matrix<dim<33>, dim<5>, int>("b")
            let a2 = Matrix<dim<33>, dim<6>, int>("a")
            a + b
        Assert.Equal("", defaultGraph.NameScope)    
        Assert.Equal("mops/Add_0", msum.Name)

    [<Fact>]
    let ``Can multiply matrices`` () =
        defaultGraph <- TensorGraph<N<5>, N<1>>()
        Assert.Equal("", defaultGraph.NameScope)
        let msum = 
            use mops = scope "mops"
            Assert.Equal("mops", defaultGraph.NameScope)
            let a = Matrix<dim<33>, dim<5>, int>("a")
            let b = Matrix<dim<5>, dim<60>, int>("b")
            let a2 = Matrix<dim<33>, dim<6>, int>("a")
            a * b
        Assert.Equal("", defaultGraph.NameScope)    
        Assert.Equal("mops/MatMul_0", msum.Name)


