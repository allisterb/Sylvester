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

type Input(graph:TensorGraph<_,_>, name:string, ?shape:int64[]) = 
    inherit Node(graph, name, graph._Graph.Placeholder(TF_DataType.TF_BFLOAT16), [])