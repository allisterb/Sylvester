namespace Sylvester.Tests

open System
open Xunit
open Sylvester.Base10
open Sylvester.Base10Digits
open Sylvester.Arithmetic
open System.Numerics

module ArithmeticProvider = 
    type X = N<100>

    [<Fact>]
    let ``Can construct`` () =
        let x = X()
        Assert.NotNull x

    
