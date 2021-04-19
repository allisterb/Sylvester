namespace Sylvester.Tests.CAS

module MaximaTests = 

    open System
    open Xunit

    open Sylvester

    [<Fact>]
    let ``Can start maxima process`` () =
        let m = Maxima.start "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"
        Assert.True m.Initialized
        let g = 
            match Maxima.send m "partfrac ( 1/(x^2*(x^2 + 1)), x);" with
            | Success r -> true
            | Failure _ -> false
        Assert.True g