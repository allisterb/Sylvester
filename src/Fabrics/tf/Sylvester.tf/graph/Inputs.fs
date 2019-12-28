namespace Sylvester.tf

open TensorFlow

open Sylvester.Arithmetic
open Sylvester.Graphs

module Nodes = 

    type Node with
        static member Input<'t>(graph:TensorGraph<_,_>, name:string, ?shape:int64[]) =
            let t = graph._Graph.Placeholder(dataType<'t>, defaultArg shape null, name) in
            Node(graph, name, t, [new Edge(graph, name, t, dataType<'t>, defaultArg shape null])
            

    
