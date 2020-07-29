namespace Sylvester.Tests.CAS

open FSharp.Quotations

open Xunit
open MathNet.Symbolics
open MathNet.Symbolics.Operators

open Sylvester

module MathNet = 
    let var<'t> = Unchecked.defaultof<'t>

    [<Fact>]
    let ``My test`` () =
        let x = symbol "x"
        let v = [Var("x", typeof<float>)]
       
        let res = MathNetExpr.toQuotation (x + 2) v 
        Assert.True(true)
