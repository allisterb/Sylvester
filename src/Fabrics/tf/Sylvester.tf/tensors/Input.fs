namespace Sylvester.Tensors

open System
open System.Linq

open TensorFlow

open Sylvester
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Collections
open Sylvester.Graphs
open Sylvester.Tensors
open Sylvester.tf

type Input(graph:IGraph, dt:TF_DataType, name:string, ?shape:int64[]) =
    inherit Tensor(graph, dt, name, defaultArg shape null)
    
    interface IGraphInput with
        member x.Graph = graph
        member x.Name = name
        member x._Type = Convert.ToInt64(int dt)
    
    override x.TFOutput = x.TFGraph.Placeholder(dt, defaultArg shape null)
     
type Input<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1 when 'd10 :> Base10Digit and 'd9 :> Base10Digit 
and 'd8 :> Base10Digit and 'd7 :> Base10Digit and 'd6 :> Base10Digit
and 'd5 :> Base10Digit and 'd4 :> Base10Digit and 'd3 :> Base10Digit and 'd2 :> Base10Digit 
and 'd1 :> Base10Digit>(graph:IGraph, dt:TF_DataType, name:string, shape:int64[]) =
    inherit Input(graph, dt, name, shape)

    interface IPartialShape<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1> with
        member x.Rank = N10<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>()
