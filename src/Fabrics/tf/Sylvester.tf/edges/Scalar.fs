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
module Scalar =
    type Scalar<'t when 't: struct and 't:> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>
        (graph:ITensorGraph, name:string, head:Node, output:int) = 
        inherit Tensor<zero, 't>(graph, name, head, output, [|0L|])
        interface IScalar
        
        new(name:string) = 
            let g = defaultGraph
            let shape = [|0L|]
            new Scalar<'t>(g, name, new Node(g, "Placeholder", tf(g).Placeholder(dataType<'t>, shape), []), 0)

        static member (+)(l:Scalar<'t>, r:Scalar<'t>) = 
            let edgeName = l.TensorGraph.MakeName(sprintf "Add_%s_%s" l.Name r.Name) 
            let op = add l.Head r.Head
            let node = Node(l.TensorGraph, op.Name, op.Op.[0], [l :> Edge; r:>Edge])
            new Scalar<'t>(l.TensorGraph, edgeName, node, 0)
            
        static member (-)(l:Scalar<'t>, r:Scalar<'t>) = 
            let edgeName = l.TensorGraph.MakeName(sprintf "Sub_%s_%s" l.Name r.Name) 
            let op = sub l.Head r.Head
            let node = Node(l.TensorGraph, op.Name, op.Op.[0], [l :> Edge; r:>Edge])
            new Scalar<'t>(l.TensorGraph, edgeName, node, 0)
          
        static member (*)(l:Scalar<'t>, r:Scalar<'t>) = 
            let edgeName = l.TensorGraph.MakeName(sprintf "Mul_%s_%s" l.Name r.Name) 
            let op = mul l.Head r.Head
            let node = Node(l.TensorGraph, op.Name, op.Op.[0], [l :> Edge; r:>Edge])
            new Scalar<'t>(l.TensorGraph, edgeName, node, 0)

        static member (/)(l:Scalar<'t>, r:Scalar<'t>) = 
            let edgeName = l.TensorGraph.MakeName(sprintf "Div_%s_%s" l.Name r.Name) 
            let op = div l.Head r.Head
            let node = Node(l.TensorGraph, op.Name, op.Op.[0], [l :> Edge; r:>Edge])
            new Scalar<'t>(l.TensorGraph, edgeName, node, 0)