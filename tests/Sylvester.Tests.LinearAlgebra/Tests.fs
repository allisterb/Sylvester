namespace Sylvester.Tests

module LinearAlgebra = 
    open Xunit

    open Sylvester
    open Arithmetic

    
    let x,y,z = realvar3 "x" "y" "z"

    [<Fact>]
    let ``Can construct matrices`` () =
        let m1 = Mat<dim<1>, dim<3>>([| [|4.;5.;6. |] |])
        Assert.NotNull m1

        let m2 = MatZ<dim<2>, dim<3>> [| [|4; 5; 6|]; [|7;8;9|] |]
        Assert.NotNull m2
        ()

    [<Fact>]
    let ``Can add int matrices``() =
        let Ar = Matrix<dim<2>, dim<3>, int> [| [|4; 5; 6|]; [|7;8;9|] |] 
        Assert.NotNull(Ar + Ar)
        Assert.Equal(Ar + Ar, Matrix<dim<2>, dim<3>, int> [| [|8; 10; 12|]; [|14; 16; 18|] |] )

    


        
