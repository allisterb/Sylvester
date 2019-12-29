namespace Sylvester.tf

open System
open System.Collections.Generic;
open System.Runtime.CompilerServices

open TensorFlow

open Sylvester
open Sylvester.Arithmetic
open Sylvester.Collections
open Sylvester.Graphs
open Sylvester.Tensors

type Input<'t>(graph:TensorGraph<_,_>, name:string, ?shape:int64[]) = 
    inherit Edge(graph, name, Node(graph, "Placeholder", graph._Graph.Placeholder(TF_DataType.TF_BFLOAT16), []).Op.[0], dataType<'t>, defaultArg shape null)

[<AutoOpen>]
module Inputs =
    type TensorGraph<'input, 'output when 'input :> Number and 'output :> Number > with
        member x.AddInput(input:Input<'t>) = x.AddEdge(input)

    let addInput (graph:TensorGraph<_,_>) (name:string) (shape:int64[] option) = graph.AddEdge(Input(graph, name, defaultArg shape null))