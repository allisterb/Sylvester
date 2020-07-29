namespace Sylvester.Tests.CAS

open System.Numerics

open FSharp.Quotations

open Xunit
open MathNet.Symbolics
open MathNet.Symbolics.Operators

open Sylvester

module MathNet = 
    let x, y, z = symbol "x", symbol "y", symbol "z"

    [<Fact>]
    let ``Can transform basic real polynomial expression to quotation`` () =
        let v = [Var("x", typeof<float>); Var("y", typeof<float>)]
        let res = MathNetExpr.toQuotation (((x **2) * 5 ) / y + 2) v 
        Assert.NotNull res

    [<Fact>]
    let ``Can transform basic complex expression to quotation`` () =
        let v = [Var("x", typeof<C>); Var("y", typeof<C>)]
        let res = MathNetExpr.toQuotation ((x * new Complex(5., 4.)) / ((y**3) + new Complex(2., 1.))) v 
        Assert.NotNull res

