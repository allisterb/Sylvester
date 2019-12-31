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


type Scalar<'t when 't:> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable>(graph:TensorGraph<_,_>, name:string, head:Node, output:int) = 
    inherit Tensor<zero, 't>(graph :> IGraph, name, head, output, [|0L|])
    interface IScalar
    member x.TensorGraph = graph

    new(name:string) = 
        let g = TensorGraph<zero, zero>.DefaultGraph
        let shape = [|0L|]
        new Scalar<'t>(g, name, new Node(g, "Placeholder", g._Graph.Placeholder(dataType<'t>, shape), []), 0)

        static member (+) (l:Scalar<'t>, r:Scalar<'t>) = l.TensorGraph.Add(l, r)
