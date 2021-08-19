namespace Sylvester.Tests.Prover

open Sylvester 

module ETests =    
    open Xunit
    
    open Sylvester
 
    module Vars = 
        let P,Q,N,S = boolvar4 "P" "Q" "N" "S"
        let x,y = boolvar2 "x" "y"

    [<Fact>]
    let ``Can parse propositional logic``() =
        let xx = TPTP.parse_file "TPTP\\Prop1.p"
        Assert.NotNull xx
        //Assert.NotNull(TPTP.parse_file "C:\\cygwin64\\home\\Allister\\eprover\\EXAMPLE_PROBLEMS\\TPTP\\BOO006-1.p")