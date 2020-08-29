namespace Sylvester.Tests

module LinearAlgebra = 

    open System
    open Xunit

    open Sylvester
    open Sylvester.Arithmetic

    let x,y,z = var3<float>

    [<Fact>]
    let ``Can construct matrices`` () =
        let m1 = Mat<one, three>([4.;5.;6.])
        Assert.NotNull m1
        ()

    [<Fact>]
    let ``Can add int matrices``() =
        let Ar = Matrix<two, three, int> <@[ [4; 5; 6]; [7;8;9] ]@> 
        Assert.NotNull(Ar + Ar)

        let Ar2 = Mat<two, three> <@[ [x; y; 8.]; [7.;8.;9.] ]@>
        Assert.NotNull (Ar2.ToString())

    [<Fact>]
    let ``Can get numeric values for symbolic matrix``() =
        let Ar2 = Mat<two, three> <@[ [x; y; 8.]; [7.;8.;z] ]@>
        Assert.NotEmpty Ar2._Array



        
