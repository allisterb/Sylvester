namespace Sylvester.Tests.Prover

open Sylvester 

module ETests =    
    open Xunit
    
    open Sylvester
 
    module Vars = 
        let P,Q,N,S = var4<bool>
        let x,y = var2<bool>

    [<Fact>]
    let ``Can parse propositional logic``() =
        let xx = TPTP.parse_file "TPTP\\Prop1.p"
        Assert.NotNull xx
        //Assert.NotNull(TPTP.parse_file "C:\\cygwin64\\home\\Allister\\eprover\\EXAMPLE_PROBLEMS\\TPTP\\BOO006-1.p")