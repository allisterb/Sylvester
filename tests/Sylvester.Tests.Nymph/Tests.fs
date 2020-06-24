namespace Sylvester.Tests

open System
open Xunit

open Sylvester
open Nymph

module NymphTests =

    [<Fact>]
    let ``My test`` () =
        let i = getIntent "Prove the formula p = q = not ( p = q)"
        let I =  i.Res.Entities
        Assert.NotEmpty I
