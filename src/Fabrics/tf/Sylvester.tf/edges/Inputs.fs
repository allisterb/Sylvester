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


[<AutoOpen>]
module Inputs =
    type TensorGraph<'input, 'output when 'input :> Number and 'output :> Number> with
        member graph.AddInput<'t>(name:string) = graph.AddEdge(Edge(graph, name, Node(graph, "Placeholder", graph._Graph.Placeholder(dataType<'t>), []), 0, dataType<'t>))
            
            