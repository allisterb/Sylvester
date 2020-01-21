namespace Sylvester.Tests.tf

open System
open System.IO
open System.Linq

open Xunit

open Sylvester
open Sylvester.Arithmetic
open Sylvester.tf
open Sylvester.Tests


type GraphDefTests() =
    inherit BaseTest()

    [<Fact>]
    let ``Can create graph definition`` () = 
        let def = GraphDef("graph1.pb")
        Assert.NotEmpty(def.Nodes())