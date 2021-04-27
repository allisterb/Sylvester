namespace Sylvester.Tests.Solver

open FSharp.Quotations

open Xunit
  
open Sylvester 
open Z3

module Z3Tests =      
    [<Fact>]
    let ``Can create arithmetic expr``() =
        let x = var<int>
        let ctx = create_ctx()
        let e = create_arith_expr ctx <@ x + 2 @>
        Assert.NotNull e

    [<Fact>]
    let ``Can create bool expr``() =
        let x = var<int>
        let n = var<bool>
        let ctx = create_ctx()
        let e = create_bool_expr ctx <@ x > x + 2 @>
        Assert.NotNull e


 