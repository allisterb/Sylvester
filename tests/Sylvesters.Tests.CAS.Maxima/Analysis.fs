namespace Sylvester.Tests.CAS

module Analysis = 

    open System
    open Xunit

    open Sylvester
    open Sylvester.CAS

    do Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"

    [<Fact>]
    let ``Can differentiate``() =
        let x = LatinVars.x<real>
        let y = LatinVars.y<real>
        let a = LatinVars.a<real>
        let d0 = Analysis.diff <@ %x ** 2.@> x 
        Assert.NotNull d0
        let d1 = Analysis.diff <@ (%a * %x - %x ** 2. - %x ** 3.) @> x 
        Assert.NotNull d1