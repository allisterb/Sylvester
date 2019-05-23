namespace Sylvester.Tests

open System
open Xunit
open Sylvester.Base10
open Sylvester.Base10Digits
open Sylvester.Arithmetic
open System.Numerics

module ArithmeticProvider = 
    type X = N<100>
    [<Measure>] type DD
    type ve<'a, [<Measure>] 'g> when 'a :> N<100>() = class end
    type d = ve<X, DD>
    [<Fact>]
    let ``Can construct`` () =
        let x = X()
        let j = ve<N<100>, DD>() 
        let z = Activator.CreateInstance<N<100>>()
        Assert.NotNull x

    
