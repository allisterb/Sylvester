namespace Sylvester.Tests.tf

module OpGenTests =

    open System
    open System.Collections.Generic
    open System.Runtime.InteropServices
    open Xunit
    
    open Sylvester.tf.OpGen

    let gen = new Generator()

    [<Fact>]
    let ``Can get ops`` () =
        Assert.NotEmpty(gen.OpDefs)
        Assert.NotNull(gen.ApiDefMap)
        Assert.NotEqual(IntPtr.Zero, gen.ApiDefMap.__Instance)
        let a = gen.GetApiDef("Add")
        Assert.NotNull(a)