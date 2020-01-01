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
        let nodeName = l.TensorGraph.MakeName "Add"
        Node(graph, nodeName, ops(graph).Add(l.Op.[0], r.Op.[0]), [])
        
    let sub (l:Node) (r:Node) = 
        let graph = l.TensorGraph
        let nodeName = l.TensorGraph.MakeName "Sub"
        Node(graph, nodeName, ops(graph).Sub(l.Op.[0], r.Op.[0]), [])

    let mul (l:Node) (r:Node) = 
        let graph = l.TensorGraph
        let nodeName = l.TensorGraph.MakeName "Mul"
        Node(graph, nodeName, ops(graph).Add(l.Op.[0], r.Op.[0]), [])

    let div (l:Node) (r:Node) = 
        let graph = l.TensorGraph
        let nodeName = l.TensorGraph.MakeName "Div"
        Node(graph, nodeName, ops(graph).Add(l.Op.[0], r.Op.[0]), [])
