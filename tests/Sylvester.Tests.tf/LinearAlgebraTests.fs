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
        defaultGraph <- TensorGraph<n<5>, n<1>>()
        Assert.Equal("", defaultGraph.NameScope)
        let msum = 
            use mops = scope "mops"
            Assert.Equal("mops", defaultGraph.NameScope)
            let a = Mat<dim<33>, dim<5>>("a")
            let b = Mat<dim<33>, dim<5>>("b")
            let a2 = Mat<dim<33>, dim<6>>("a")
            a + b
        Assert.Equal("", defaultGraph.NameScope)    
        Assert.Equal("mops/Add_0", msum.Name)


