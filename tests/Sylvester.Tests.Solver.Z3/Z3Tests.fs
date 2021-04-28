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
        let s = new Z3Solver()
        
        Assert.NotNull <| create_arith_expr s <@ %x + 2 @>
        Assert.NotNull <| create_arith_expr s <@ %x' + %y' * 2. @>

    [<Fact>]
    let ``Can create bool expr``() =
        let x = var'<int> "x"
        let b = var'<bool> "b"
        use s = new Z3Solver()
        Assert.NotNull <| create_bool_expr s <@ %x > %x + 2 @>
        Assert.NotNull <| create_bool_expr s <@ %b ==> false @>

    [<Fact>]
    let ``Can check sat``() =
        let x = var'<real> "x"
        let y = var'<real> "y"
        let b = var'<bool> "b"
       
        let solver = new Z3Solver()
        let a = <@ [%x = 6.; %x > 5.] @>  
        Assert.True <| (Option.isSome <| check_sat_model solver a)

        let A = var'<Set<real>> "A"
        let B = var'<Set<real>> "B"
        let C = var'<Set<real>> "C"
        Assert.False <| check_sat solver <@ [%A |*| (%B |+| %C) <> ((%A |*| %B) |+| (%A |*| %C))] @>

    [<Fact>]
    let ``Can solve``() =
         let x = var'<int> "x"
         let y = var'<int> "y"
         let b = var'<bool> "b"
         
         let solver = new Z3Solver()
         let a = <@ [%x = 5; %y = %x + 2] @>  
         let sol = check_sat_model solver a
         Assert.True sol.IsSome
         let rr = _get_int_var_model sol.Value
         Assert.NotEmpty rr

         let qx = var'<Rational> "qx"
         let qy = var'<Rational> "qy"
         let sol2 = check_sat_model solver <@[%qx = 1Q ]@>
         Assert.True sol2.IsSome

    [<Fact>]
    let ``Can optimize``() =
         let x = var'<int> "x"
         let y = var'<int> "y"
         let b = var'<int> "b"
         let s = new Z3Solver()
         opt_assert_hard s <@ [%x > 0; %y > 0; %x * %x + %y * %y < 10] @>
         let h = opt_maximize s <@ %x * %x + %y * %y@>
         
         Assert.True <| opt_check_sat s
         let m = opt_get_int_var_model s
         Assert.NotNull m
         

 