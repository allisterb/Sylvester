namespace Sylvester.Tests.NLU

open Sylvester.NLU.Wit

module WitTests =

    open System
    open Xunit

    [<Fact>]
    let ``Can create client`` () =
        let t = System.Environment.GetEnvironmentVariable("WIT")
        Assert.NotNull t
        let c = new Client(t)
        Assert.NotNull c
