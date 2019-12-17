namespace Sylvester.Tests.tf

module OpGenTests =

    open System
    open System.Collections.Generic;
    open Xunit
    
    open Sylvester.tf.OpGen

    [<Fact>]
    let ``Can get ops list`` () =
        let defs = Generator.GetOpsList();
        Assert.NotEmpty(defs)
