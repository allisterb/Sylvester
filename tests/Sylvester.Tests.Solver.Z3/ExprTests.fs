namespace Sylvester.Tests.Solver

open FSharp.Quotations

open Xunit
  
open Sylvester 
open Z3


module Z3Tests =      
    [<Fact>]
    let ``Can create arithmetic expr``() =
        let x = var'<int> "x"
        let x' = var'<real> "xx"
        let y' = var'<real> "y"
        let ctx = create_ctx()
        
        Assert.NotNull <| create_arith_expr ctx <@ %x + 2 @>
        Assert.NotNull <| create_arith_expr ctx <@ %x' + %y' * 2. @>

    [<Fact>]
    let ``Can create bool expr``() =
        let x = var'<int> "x"
        let b = var'<bool> "b"
        let ctx = create_ctx()
        
        Assert.NotNull <| create_bool_expr ctx <@ %x > %x + 2 @>
        Assert.NotNull <| create_bool_expr ctx <@ %b ==> false @>

    [<Fact>]
    let ``Can check sat``() =
        let x = var'<real> "x"
        let y = var'<real> "y"
        let b = var'<bool> "b"
        let ctx = create_ctx()
        let solver = create_solver ctx
        let a = <@ [%x = 6.; %x > 5.] @>  
        Assert.True <| check_sat ctx solver a

    [<Fact>]
    let ``Can solve``() =
         let x = var'<int> "x"
         let y = var'<int> "y"
         let b = var'<bool> "b"
         let ctx = create_ctx()
         let solver = create_solver ctx
         let a = <@ [%x = 5; %y = %x + 2] @>  
         let sol = check_sat_model ctx solver a
         Assert.True sol.IsSome
         let rr = get_int_model_consts sol.Value
         Assert.NotEmpty rr
        
    [<Fact>]
    let ``Can get tactics``() =
         let t =  tactics
         Assert.NotEmpty t



 