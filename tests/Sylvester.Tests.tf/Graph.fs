namespace Sylvester.Tests.tf

module GraphTests =

    open System

    open Xunit
    
    open Sylvester.tf

    do Sylvester.Api.SetDefaultLoggerIfNone()

    [<Fact>]
    let ``Can create graph`` () =
        Assert.NotNull(new Graph())
