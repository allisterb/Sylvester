module Tests

open System
open Xunit

open Sylvester.Arithmetic
open Sylvester.tf

[<Fact>]
let ``My test`` () =
    let g = TensorGraph<n<15>, n<1>>("g")
    Assert.NotNull(g)
    Assert.Equal("g", g.NameScope)
