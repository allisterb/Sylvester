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
        let s = Z3Solver()
        
        Assert.NotNull <| create_arith_expr s <@ %x + 2 @>
        Assert.NotNull <| create_arith_expr s <@ %x' + %y' * 2. @>

    [<Fact>]
    let ``Can create bool expr``() =
        let x = var'<int> "x"
        let b = var'<bool> "b"
        let s = Z3Solver()
        Assert.NotNull <| create_bool_expr s <@ %x > %x + 2 @>
        Assert.NotNull <| create_bool_expr s <@ %b ==> false @>

    [<Fact>]
    let ``Can check sat``() =
        let x = var'<real> "x"
        let y = var'<real> "y"
        let b = var'<bool> "b"
       
        let solver = Z3Solver()
        let a = <@ [%x = 6.; %x > 5.] @>  
        Assert.True <| check_sat solver a

    [<Fact>]
    let ``Can solve``() =
         let x = var'<int> "x"
         let y = var'<int> "y"
         let b = var'<bool> "b"
         
         let solver = Z3Solver()
         let a = <@ [%x = 5; %y = %x + 2] @>  
         let sol = check_sat_model solver a
         Assert.True sol.IsSome
         let rr = get_int_var_model sol.Value
         Assert.NotEmpty rr

         let qx = var'<Rational> "qx"
         let qy = var'<Rational> "qy"
         let sol2 = check_sat_model solver <@[%qx = 1Q ]@>
         Assert.True sol2.IsSome


 