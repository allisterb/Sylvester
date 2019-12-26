namespace Sylvester.Tensors

open System
open System.Linq

open TensorFlow

open Sylvester
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Collections
open Sylvester.Graphs
open Sylvester.Tensors

type Tensor(graph:IGraph, dt:TF_DataType, name:string, shape:int64[]) = class end