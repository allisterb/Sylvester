namespace Sylvester.Tests

open System
open Xunit

open Sylvester
open Economics
open Integrals

module Economics =



    do CAS.Maxima.init "C:\\MathTools\\maxima-5.44.0\\bin\\maxima.bat"

    [<Fact>]
    let ``My test`` () =
        //Declare 2 real variables
        let p,q = realvar2 "p" "q"
        //Declare 3 real constants representing the price of sugar, price of chocolate, and consumer income respectively
        let ps, pc, Y = realconst3 "p_s" "p_c" "Y"
    
        let QD = demandfun "Q_d" (8.56 - p - 0.3 * ps + 0.1 * Y)
    
        let QD1 = fix {|p_s=0.2; Y=35.|} QD
        Assert.NotNull QD1
    
    [<Fact>]
    let ``My test 2`` () =
        //Declare 2 real variables
        let g = realfun_l <@fun x -> 1. + x ** 2. @> |> upper_riemann_sum -2. 2. 3
        Assert.True (g > 0.)