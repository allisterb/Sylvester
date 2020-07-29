namespace Sylvester.Tests.CAS

open FSharp.Quotations

open Xunit
open MathNet.Symbolics
open MathNet.Symbolics.Operators

open Sylvester

module MathNet = 
    let var<'t> = Unchecked.defaultof<'t>

    [<Fact>]
    let ``Can transform basic polynomial expression to quotation`` () =
        let x, y = symbol "x", symbol "y"
        let v = [Var("x", typeof<float>); Var("y", typeof<float>)]
       
        let res = MathNetExpr.toQuotation ((x * 5) / y + 2) v 
        Assert.NotNull res
