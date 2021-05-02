namespace Sylvester.Tests

open System
open Xunit

open Sylvester
open LP

module LPTests = 

    [<Fact>]
    let ``Can get max`` () =
        //let x = LatinVars.x<int>
        //let y = LatinVars.y<int>
        //let a = max <@ %x * %x + %y * %y@> <@ [%x > 0; %y > 0; %x * %x + %y * %y < 10] @>
        //Assert.True opt_
        //Assert.True a.IsSome

        let x' = LatinVars.x<rat>
        let y' = LatinVars.y<rat>
        let b = max <@ %x' * %x' + %y' * %y'@> <@ [%x' > 0Q; %y' > 0Q; %x' * %x' + %y' * %y' < 10Q] @>
        Assert.True b.IsSome