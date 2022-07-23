namespace Sylvester.Tests.CAS

module Algebra = 

    open System
    open Xunit

    open Sylvester
    open Sylvester.CAS

    do Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"
    
    [<Fact>]
    let ``Can start maxima process`` () =
        let m = Maxima.start "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"
        Assert.True m.Initialized
        let g = 
            match Maxima.send m "partfrac ( 1/(x^2*(x^2 + 1)), x);" with
            | Success r -> true
            | Failure _ -> false
        Assert.True g

    [<ReflectedDefinition>]
    let fo x = x ** 2. + 2. * x

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

        let x = LatinVars.x<real>
        let a = LatinVars.a<real>
        
        let f''' = Analysis.limit <@ (fo(%x + %a) - fo %x) / %a @> a <@ 0. @>
        Assert.NotNull f'''

    [<Fact>]
    let ``Can solve``() =
        let x, y = intvar2 "x" "y"
        let soln = Algebra.solve y <@[3 * %x + 5 * %y = 120 ]@>
        let xx = MathNet.Symbolics.Infix.parse "(3*x-120)/5"
        
        let r = xx |> function | Ok e -> Some <| MathNetExpr.toQuotation<int> (get_vars x) e | _ -> None
        Assert.True <| Option.isSome r