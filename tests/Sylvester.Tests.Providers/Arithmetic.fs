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
    
    type V<'n>() = class end

  
    


    
    
    [<Fact>]
    let ``Can construct`` () =
        let x = X()
        let j = ve<N<100>, DD>() 
        let z = Activator.CreateInstance<N<100>>()
        Assert.NotNull x
        let h = new N<100>()
        //let z = new N5<
        Assert.NotNull h

    
