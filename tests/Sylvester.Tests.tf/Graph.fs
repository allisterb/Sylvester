namespace Sylvester.Tests.tf

open System

open Xunit

open Sylvester
open Sylvester.tf
open Sylvester.Tests

type GraphTests() =
    inherit BaseTest()

    [<Fact>]
    let ``Can create graph`` () =
      
        let g = new Graph()
        
        Assert.NotNull(g)
        Assert.True(g.Initialized)
