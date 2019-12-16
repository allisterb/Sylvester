namespace Sylvester.Tests.tf

module BindingsTests =

    open System
    open Xunit

    [<Fact>]
    let ``Can get Version`` () =
        Assert.NotNull(TensorFlow.c_api.TF_Version())
