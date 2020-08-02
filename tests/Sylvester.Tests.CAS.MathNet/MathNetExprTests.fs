namespace Sylvester.Tests.CAS

open System.Numerics

open FSharp.Quotations

open Xunit
open MathNet.Symbolics
open MathNet.Symbolics.Operators

open Sylvester

module MathNet = 
    let x, y, z = symbol "x", symbol "y", symbol "z"
    let var<'t> = Unchecked.defaultof<'t>

    [<Fact>]
    let ``Can transform basic real polynomial expression`` () =
        let v = [Var("x", typeof<float>); Var("y", typeof<float>)]
        let res = MathNetExpr.toQuotation v (((x **2) * 5 ) / y + 2)  
        Assert.NotNull res

    [<Fact>]
    let ``Can transform basic complex expression`` () =
        let v = [Var("x", typeof<C>); Var("y", typeof<C>)]
        let res = MathNetExpr.toQuotation v ((x * new Complex(5., 4.)) / ((y**3) + new Complex(2., 1.)))  
        Assert.NotNull res

    [<Fact>]
    let ``Can transform basic quotation``() =
        let a = var<float>
        let v = [Var("a", typeof<float>)]
        let m = MathNetExpr.fromQuotation <@ 4. * (a ** 2.) + a * (a + 6.)  @>
        let q = 
            m 
            |> MathNet.Symbolics.Algebraic.expand 
            |> MathNetExpr.toQuotation v
            //|> MathNetExpr.t (toSymbol a)
        Assert.NotNull q