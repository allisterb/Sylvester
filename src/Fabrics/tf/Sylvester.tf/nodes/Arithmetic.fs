namespace Sylvester.tf

open System
open System.Collections.Generic;
open System.Runtime.CompilerServices


open Sylvester
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Collections
open Sylvester.Graphs

[<AutoOpen>]
module Arithmetic =
    let add (l:Node) (r:Node) = 
        let graph = l.TensorGraph
        let edgeName = l.TensorGraph.MakeName(sprintf "Sub_%s_%s" l.Name r.Name)
        let nodeName = "Sub"
        Node(graph, nodeName, ops(graph).Add(l.Op.[0], r.Op.[0]), [])
        

        //let add(l:Tensor<'r, 't>, r:Node) = Node(l.TensorGraph, "Add", ops(l).Add(l.Op.[0], r.Op.[0]),
        //member graph.Sub(l:Tensor<'r, 't>, r:Tensor<'r, 't>) = Node(graph, "Sub", graph._Graph.Sub(l.Output, r.Output), [l:>Edge;r:>Edge])
        //member graph.Mul(l:Tensor<'r, 't>, r:Tensor<'r, 't>) = Node(graph, "Mul", graph._Graph.Mul(l.Output, r.Output), [l:>Edge;r:>Edge])
        //member graph.Div(l:Tensor<'r, 't>, r:Tensor<'r, 't>) = Node(graph, "Div", graph._Graph.Mul(l.Output, r.Output), [l:>Edge;r:>Edge])
