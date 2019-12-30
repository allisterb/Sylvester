namespace Sylvester.Tests.Arithmetic.FixedPoint

module ConstraintTests = 

    open System
    open System.Reflection
    open Xunit
    open Sylvester.Arithmetic
    open Sylvester.Arithmetic.N10
       
    [<Fact>]
    let ``Can constraint fixed-point number``() = 
        checkgt(five, one)
