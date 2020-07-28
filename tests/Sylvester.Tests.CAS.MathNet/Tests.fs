namespace Sylvester.Tests.CAS

open Sylvester
open MathNet.Symbolics

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns


module MathNet = 
    open System
    open Xunit

    let var<'t> = Unchecked.defaultof<'t>

    [<Fact>]
    let ``My test`` () =
        let j = Infix.parseOrThrow "x + 1"
        
        let x = var<float>
        let q = <@ x @>
        let v = [Var("x", typeof<float>)]
       
        let res = MathExpr.toQuotation j v typeof<float>
        Assert.True(true)
