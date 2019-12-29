namespace Sylvester.tf

open System
open System.Collections.Generic;
open System.Runtime.CompilerServices

open TensorFlow

open Sylvester
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Collections
open Sylvester.Graphs
open Sylvester.Tensors


[<AutoOpen>]
module Inputs =
    type TensorGraph<'input, 'output when 'input :> Number and 'output :> Number> with
        member graph.Input<'t>(name:string) = new Edge(graph, name, new Node(graph, "Placeholder", graph._Graph.Placeholder(dataType<'t>), []), 0, dataType<'t>)
           
       
            