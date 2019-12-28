namespace Sylvester.Tests.tf.Api

open System
open System.IO;
open System.Net;

open Xunit

open Sylvester.Tests

open TensorFlow

type TFGraphTests() =
    inherit BaseTest()

    do if not <| File.Exists("graph1.pb") then
        let c = new WebClient() in
            c.DownloadFile("https://github.com/kindlychung/demo-load-pb-tensorflow/raw/master/latest.pb", "graph1.pb")

    [<Fact>]
    let ``Can import graph from file``() =
        Assert.True(File.Exists("graph1.pb"))
        let (graph, ops, status) = TF_Graph.Import("graph1.pb", c_api.TF_NewImportGraphDefOptions());
        Assert.Equal(TF_Code.TF_OK, tf_status.TF_GetCode(status))
        Assert.NotNull(graph)
        Assert.NotNull(ops) 
        Assert.NotEmpty(ops)