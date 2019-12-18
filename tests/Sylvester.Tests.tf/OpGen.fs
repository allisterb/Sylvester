namespace Sylvester.Tests.tf

module OpGenTests =

    open System
    open System.Collections.Generic
    open System.Linq
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

    [<Fact>]
    let ``Can generate op code`` () =
        let ad = gen.OpDefs.Single(fun o -> o.name = "Add")
        gen.Generate(ad) |> ignore
        Assert.NotNull(gen.Output)
