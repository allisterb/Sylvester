namespace Sylvester.Tests.CAS

module MaximaTests = 

    open System
    open Xunit

    open Sylvester

    [<Fact>]
    let ``Can start maxima process`` () =
        let m = Maxima.start "C:\\maxima-5.43.2\\bin\\maxima.bat"
        Assert.True m.Initialized
        let g = 
            match Maxima.send m "2 + 2;" with
            | Success r -> true
            | Failure _ -> false
        Assert.True g