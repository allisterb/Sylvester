namespace Sylvester.Tests.Arithmetic

module Dimensions = 

    open System
    open System.Reflection
    open Xunit
    open Sylvester.Arithmetic
    open Sylvester.Arithmetic.N10
       
    [<Fact>]
    let ``Can construct dim``() = 
        let g = Rank3(number<N0>, number<N0>, number<N0>).GetType()
        Assert.NotNull g

