namespace Sylvester.Tests.tf

module BindingsTests =

    open System
    open Xunit
    open TensorFlow

    [<Fact>]
    let ``Can get Version`` () =
        Assert.NotNull(c_api.TF_Version())
