namespace Sylvester.Tests.Expressions

module FsExpr = 

    open Xunit

    open Sylvester
    open FSharp.Quotations

    [<Fact>]
    let ``Can get vars``() =
        let f = <@fun x -> (2.*x**3. + 1.) @>
        let v = get_vars <| body' f
        Assert.NotEmpty v