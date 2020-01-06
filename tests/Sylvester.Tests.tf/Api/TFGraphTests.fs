namespace Sylvester.Tests.tf.Api

open System
open System.IO;
open System.Net;

open Xunit

open Sylvester.Tests

open TensorFlow

type TFGraphTests() =
    inherit BaseTest()
    let c = new WebClient()
    do 
        if not <| File.Exists("graph1.pb") then c.DownloadFile("https://github.com/kindlychung/demo-load-pb-tensorflow/raw/master/latest.pb", "graph1.pb")
           
    [<Fact>]
    let ``Can import graph from file``() =
        Assert.True(File.Exists("graph1.pb"))
        let (graph, _ops, status) = TF_Graph.Import("graph1.pb", c_api.TF_NewImportGraphDefOptions());
        Assert.Equal(TF_Code.TF_OK, tf_status.TF_GetCode(status))
        Assert.NotNull(graph)
        Assert.NotNull(_ops) 
        Assert.NotEmpty(_ops)
        let ops = Seq.map (fun op -> c_api.TF_OperationName op) _ops
        Assert.Contains("input", ops)
        Assert.Contains("final_result", ops)

        let def = TF_Graph.Import("graph1.pb")
        Assert.NotNull(def)
        Assert.NotEmpty(def.Node)
        


      