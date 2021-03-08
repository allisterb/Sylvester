namespace Sylvester.Tests.FunScript

open System
open Xunit

open FunScript
open FunScript.Bindings
open FunScript.Bindings.D3

module D3Tests = 
    //let u2_1<'t> = U2<'t,_>.Case1
    [<JS>]
    let scale(d:float) =  
        let r (a:int) (b:string) = {| f= a; x = b|}
        d3.scaleLinear().domain([|U2.Case1 d; U2.Case1 130.0|]).range([|0.0; 960.0|])
     

    [<Fact>]
    let ``Can create D3 axis`` () =    
        let s = compile <@ scale @>
        let u = s
        Assert.NotNull s
        let ss = compile <@ [ {|append="circle"; r = 50; cx  =50; cy = 50; fill="red" |} ] @>
        Assert.NotNull ss
        
                   
