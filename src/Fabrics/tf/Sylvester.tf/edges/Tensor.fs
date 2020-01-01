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
module Tensor = 
    /// Tensor of unknown rank and dimensions
    type Tensor<'t when 't:> ValueType and 't : struct  and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>
            (graph:ITensorGraph, name:string, head:Node, output:int, ?shape:int64[]) as this = 
        inherit Edge(graph, name, head, output, dataType<'t>, defaultArg shape null)
        do graph.Add this

        new(name:string, ?shape:int64[]) = 
            let g = defaultGraph
            new Tensor<'t>(g, name, new Node(g, "Placeholder", tf(g).Placeholder(dataType<'t>, defaultArg shape null), []), 0, defaultArg shape null)
            
    /// Tensor of known rank but unknown dimensions
    type Tensor<'r, 't when 'r :> Number and 't:> ValueType and 't : struct  and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>
            (graph:ITensorGraph, name:string, head:Node, output:int, ?shape:int64[]) = 
        inherit Edge<'r>(graph, name, head, output, dataType<'t>, defaultArg shape (Array.create number<'r>.IntVal 0L))
        
        do if number<'r>.IntVal > 0 && shape.IsSome && shape.Value.Length <> number<'r>.IntVal then failwith "The shape array parameter length does not match the tensor's type rank."
        
        new(name:string, ?shape:int64[]) = 
            let g = defaultGraph
            new Tensor<'r, 't>(g, name, new Node(g, "Placeholder", tf(g).Placeholder(dataType<'t>, defaultArg shape (Array.create number<'r>.IntVal 0L)), []), 0, defaultArg shape (Array.create number<'r>.IntVal 0L))
    
        //Arithmetic operators
        static member (+) (l:'x, r:'x when 'x :> Tensor<'r, 't>) = 
            let edgeName = l.TensorGraph.MakeName(sprintf "Add_%s_%s" l.Name r.Name) 
            let node = add l.Head r.Head
            node.Inputs <- [l; r]
            Activator.CreateInstance (typeof<'x>, ([|l.TensorGraph :> obj; edgeName :> obj; node :> obj; 0 :> obj|])) :?> 'x

        static member (-) (l:'x, r:'x when 'x :> Tensor<'r, 't>) = 
            let edgeName = l.TensorGraph.MakeName(sprintf "Sub_%s_%s" l.Name r.Name) 
            let node = add l.Head r.Head
            node.Inputs <- [l; r]
            Activator.CreateInstance (typeof<'x>, ([|l.TensorGraph :> obj; edgeName :> obj; node :> obj; 0 :> obj|])) :?> 'x

        static member (*) (l:'x, r:'x when 'x :> Tensor<'r, 't>) = 
            let edgeName = l.TensorGraph.MakeName(sprintf "Mul_%s_%s" l.Name r.Name) 
            let node = add l.Head r.Head
            node.Inputs <- [l; r]
            Activator.CreateInstance (typeof<'x>, ([|l.TensorGraph :> obj; edgeName :> obj; node :> obj; 0 :> obj|])) :?> 'x

        static member (/) (l:'x, r:'x when 'x :> Tensor<'r, 't>) = 
            let edgeName = l.TensorGraph.MakeName(sprintf "Div_%s_%s" l.Name r.Name) 
            let node = add l.Head r.Head
            node.Inputs <- [l; r]
            Activator.CreateInstance (typeof<'x>, ([|l.TensorGraph :> obj; edgeName :> obj; node :> obj; 0 :> obj|])) :?> 'x

        