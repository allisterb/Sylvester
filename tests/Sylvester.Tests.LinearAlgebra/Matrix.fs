namespace Sylvester.Tests

module Matrix = 
    open Xunit

    open Sylvester
    open Matrix

   
    let x,y,z = realvar3 "x" "y" "z"

    [<Fact>]
    let ``Can construct matrices`` () =
        let m1 = mat [
            [x; y] 
            [2; 4]
        ]
        Assert.NotNull m1

        let m2 = MatZ<dim<2>, dim<3>> [| [|4; 5; 6|]; [|7;8;9|] |]
        Assert.NotNull m2
        ()