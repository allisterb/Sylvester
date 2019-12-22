namespace Sylvester.Tests.tf

module GraphTests =

    open System

    open Xunit
    
    open Sylvester.tf

    [<Fact>]
    let ``Can create graph`` () =
        Assert.NotNull(new Graph())
