namespace Sylvester.Tests.Solver

open FSharp.Quotations

open Xunit
  
open Sylvester 

module Z3Tests =      
    [<Fact>]
    let ``Can create arithmetic expr``() =
        let x = var<int>
        let ctx = Z3.create_ctx()
        let e = Z3.create_arith_expr ctx <@ x + 2 @>
        Assert.NotNull e
 