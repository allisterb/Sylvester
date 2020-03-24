namespace Sylvester.Tests.Prover

module PropCalculusTests =    
    open Xunit
    
    open Sylvester
    open PropCalculus
    
    [<Fact>]
    let ``operator works`` =
        Assert.True(prop_calculus |- <@ fun p q -> p |&| q = q |&| p @>)
        
