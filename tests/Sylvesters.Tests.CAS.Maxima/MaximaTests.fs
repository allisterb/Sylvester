namespace Sylvester.Tests.CAS

module MaximaTests = 

    open System
    open Xunit

    open Sylvester
    open Sylvester.CAS

    Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"
    
    [<Fact>]
    let ``Can start maxima process`` () =
        let m = Maxima.start "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"
        Assert.True m.Initialized
        let g = 
            match Maxima.send m "partfrac ( 1/(x^2*(x^2 + 1)), x);" with
            | Success r -> true
            | Failure _ -> false
        Assert.True g

    [<Fact>]
    let ``Can get part frac``() =
        let a = LatinVars.a<int>
        let b = LatinVars.b<int>
        let f = Algebra.partfrac <@ (2 * %a)/ (%a + 3) @> a
        Assert.NotNull f
        let f' = Algebra.partfrac <@ (1 + 2)/ %b @> b
        Assert.NotNull f'
        let c = LatinVars.c<real>
        let f'' = Algebra.partfrac <@ (2.5 + 1.) / %c @> c
        Assert.NotNull f''
