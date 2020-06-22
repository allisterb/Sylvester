namespace Sylvester.Tests.NLU

open Sylvester
open Sylvester.NLU.Wit

module WitTests =

    open System
    open Xunit

    [<Fact>]
    let ``Can create client`` () =
        let t = System.Environment.GetEnvironmentVariable("WIT")
        Assert.NotNull t
        let c = new WitClient(t)
        Assert.NotNull c
        let e = c.GetMeaning("Prove that p or q = q or p");
        Assert.NotNull e

    [<Fact>]
    let ``Can parse``() =
        let a = ExprParser.parse "p and q and (q or p)"
        Assert.NotNull a