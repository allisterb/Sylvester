namespace Sylvester.Tests.Prover

open Sylvester 

module ETests =    
    open Xunit
    
    open Sylvester
 
    module Vars = 
        let P,Q,N,S = var4<bool>
        let x,y = var2<bool>

    [<Fact>]
    let ``operator works``() =
        let xx = TPTP.parse_file "TPTP\\BOO006-2.p"
        ()
        //Assert.NotNull(TPTP.parse_file "C:\\cygwin64\\home\\Allister\\eprover\\EXAMPLE_PROBLEMS\\TPTP\\BOO006-1.p")