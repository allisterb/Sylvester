namespace Sylvester.tf

open TensorFlow

open Sylvester
open Sylvester.Graphs

module Edges = 

    type Edge with
        static member Input(graph:TensorGraph<_,_,_,_>, name:string, dt:TF_DataType,  ?shape:int64[]) = 
            Edge(graph, name, graph._Graph.Placeholder(dt, defaultArg shape null, name), dt, defaultArg shape null)        


    
