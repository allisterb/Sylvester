namespace Sylvester.Tests.tf

open System
open System.IO
open System.Linq

open Xunit

open Sylvester
open Sylvester.Arithmetic
open Sylvester.tf
open Sylvester.Tests

open Google.Protobuf
open Google.Protobuf.Collections

open TensorFlow

type GraphDefnTests() =
    inherit BaseTest()

    [<Fact>]
    let ``Can create graph definition`` () = 
        let def = GraphDefn("graph1.pb")
        Assert.NotEmpty(def.Nodes())