namespace Sylvester.Tests.FunScript

open System
open Xunit

open FunScript
open FunScript.Bindings
open FunScript.Bindings.D3

module D3Tests = 
    let u2_1<'t> = U2<'t,_>.Case1
    [<JS>]
    let scale() =  
        let r() = {| f= 1; x = "foo"|}
        d3.scaleLinear().domain([|U2.Case1 10.0; U2.Case1 130.0|]).range([|0.0; 960.0|])
     

    [<Fact>]
    let ``Can create D3 axis`` () =    
        let s = compile <@ scale @>
        let u = s
        Assert.NotNull s
        
                   
