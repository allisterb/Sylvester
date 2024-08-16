namespace Sylvester.Tests

module MatrixT = 
    open Xunit

    open Sylvester
    open Dimension
    open MatrixT

    
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

    [<Fact>]
    let ``Can multiply matrices``() =
        let A = mat ``4`` ``3`` [1; 1; 1; 2; 0; 1; 1; 2; 4; 2; 2; -1] 
        let B = mat ``3`` ``2`` [3; 0; 1; 1; -1; 3]
        Assert.Equal(A * B,  mat ``4`` ``2`` [3; 4; 5; 3; 1; 14; 9; -1]) 

    [<Fact>]
    let ``Zero matrix behaves as expected``() =
        let A = mat ``4`` ``3`` [1; 1; 1; 2; 0; 1; 1; 2; 4; 2; 2; -1] 
        Assert.Equal(A + zero, A) 
        Assert.Equal(A - A, zero)
        Assert.Equal(A * mzero ``3`` ``4``, zero)
        Assert.Equal(zero, A * mzero ``3`` ``4``)

    [<Fact>]
    let ``Identity matrix behaves as expected``() =
        let A = mat ``4`` ``3`` [1; 1; 1; 2; 0; 1; 1; 2; 4; 2; 2; -1] 
        Assert.Equal(A * mident ``3``, A)
        Assert.Equal(mident ``4`` * A, A)

    [<Fact>]
    let ``Matrix powers behave as expected``() =
        let A = mat ``2`` ``2`` [-2; 4; 6; -8]
        Assert.Equal(A ^^ 3, A * A * A)

    [<Fact>]
    let ``Matrix algebra behaves as expected``() =
        let A = mat ``4`` ``3`` [1; 1; 1; 2; 0; 1; 1; 2; 4; 2; 2; -1] 
        let B = mat ``4`` ``3`` [1; 2; 1; 4; 0; 1; 1; 6; 4; 2; 2; -8]
        Assert.Equal(3 * A + 2 * B,  2 * A + B + B + A) 

   
    


        
