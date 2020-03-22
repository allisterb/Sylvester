namespace Sylvester.Tests.CAS

module MaximaTests = 

    open System
    open Xunit

    open Sylvester

    [<Fact>]
    let ``Can start maxima process`` () =
        let m = Maxima.start "C:\\maxima-5.43.2\\bin\\maxima.bat"
        Assert.True(m.Initialized)
        Assert.NotNull(Maxima.console_process m)
        Assert.NotNull(Maxima.version m)
        m.ConsoleProcess.Stop()