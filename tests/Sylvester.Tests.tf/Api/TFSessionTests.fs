namespace Sylvester.Tests.tf.Api

open System
open System.IO;
open System.Net;

open Xunit

open Sylvester.Tests

open TensorFlow

type TFSessionTests() =
    inherit BaseTest()

    [<Fact(Skip = "Session runner does not work yet")>]
    let ``Can run simple graph``() =
        let graph = c_api.TF_NewGraph()
        graph.Dependencies <- Array.empty<TF_Operation>
        let i = new Tensor<int64>([|9L|])
        let o = new Tensor<int64>([|8L|])//tf_tensor.TF_AllocateTensor(TF_DataType.TF_INT64, d, 0, 8UL )
        //let p = graph.Placeholder(TF_DataType.TF_INT64, [|1L|]);
        let c = graph.Const(i._Tensor, i.DataType)
        let  d = ref 0L
        let s = TF_Session.New(graph)
        let r = s.Run([|c|], [|tf_tensor.TF_AllocateTensor(TF_DataType.TF_INT64, d, 0, 8UL )|], [|c|], [|tf_tensor.TF_AllocateTensor(TF_DataType.TF_INT64, d, 0, 8UL )|], [|c.Oper|])
        ()
