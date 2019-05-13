module Tests

open System
open Xunit
open Sylvester.Base10
open Sylvester.Base10Digits
open Sylvester.Arithmetic

type f  = N<100>
[<Fact>]
let ``My test`` () =
    
    let d = f() 
    
    let g = One + d


    Assert.True(true)

    
