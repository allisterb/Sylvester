namespace Sylvester.Tests.Math

module Set =

    open System
    open Xunit

    open Sylvester

    [<Fact>]
    let ``Can equate predicates`` () =
        let c = Pred(fun x -> x = 0) 
        let d = Pred(fun x -> x = 0) 
        Assert.Equal(c, d) 
