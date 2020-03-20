namespace Sylvester.Tests.CAS

module MaximaTests = 

    open System
    open Xunit

    open Sylvester

    [<Fact>]
    let ``Can start maxima process`` () =
        let m = new Maxima("C:\\maxima-5.43.2\\bin\\maxima.bat")
        Assert.True(m.Initialized)
        Assert.NotEmpty(m.ConsoleProcess.Output)
        m.Session.Expect.Contains
        m.ConsoleProcess.Stop()