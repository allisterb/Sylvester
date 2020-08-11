namespace Sylvester.Tests

module LinearAlgebra = 

    open System
    open Xunit

    open Sylvester
    open Sylvester.Arithmetic

    [<Fact>]
    let ``Can construct matrices`` () =
        let m1 = Mat<one, three>([4.;5.;6.])
        Assert.NotNull m1
        ()

    [<Fact>]
    let ``Can add int matrices``() =
        let Ar = Matrix<two, three, int> <@[ [4; 5; 6]; [7;8;9] ]@> 
        let x,y,z = var3<float>
        
        Assert.NotNull(Ar + Ar)

        let Ar2 = Mat<two, three> <@[ [x; y; 8.]; [7.;8.;9.] ]@>
        Assert.NotNull (Ar2.ToString())
        
