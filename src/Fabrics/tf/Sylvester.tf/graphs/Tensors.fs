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

module Tensors = 
    /// A graph of tensor operations.
    type TensorGraph<'input, 'output when 'input :> Number and 'output :> Number> with
        member graph.Tensor = ()

    /// Tensor of unknown shape
    and Tensor<'t when 't:> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable>(graph:IGraph, name:string, head:Node, output:int) = 
        inherit Edge(graph, name, head, output, dataType<'t>)

    /// Tensor of known rank but unknown dimensions
    and Tensor<'r, 't when 'r :> Number and 't:> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable>(graph:IGraph, name:string, head:Node, output:int, shape:int64[]) = 
        inherit Edge<'r>(graph, name, head, output, dataType<'t>, shape)

    /// Scalar
    and Scalar<'t when 't:> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable>(graph:IGraph, name:string, head:Node, output:int) = 
        inherit Tensor<zero, 't>(graph, name, head, output, Array.Empty<int64>())
        interface IScalar

    /// Vector
    and Vector<'dim0, 't when 'dim0 :> Number and 't:> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable>(graph:IGraph, name:string, head:Node, output:int) =
        inherit Tensor<one, 't>(graph, name, head, output, [|number<'dim0>.Val|])
        interface IVector
        member x.Dim0:'dim0 = number<'dim0>

    /// Matrix
    and Matrix<'dim0, 'dim1, 't when 'dim0 :> Number and 'dim1 :> Number and 't:> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable>(graph:IGraph, name:string, head:Node, output:int) =
        inherit Tensor<two, 't>(graph, name, head, output, [|number<'dim0>.Val; number<'dim1>.Val|])
        interface IMatrix
        member x.Dim0 = number<'dim0>
        member x.Dim1 = number<'dim1>

    /// 3-D Tensor
    and Tensor<'dim0, 'dim1, 'dim2, 't when 'dim0 :> Number and 'dim1 :> Number and 'dim2 :> Number and 't:> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable>(graph:IGraph, name:string, head:Node, output:int) =
        inherit Tensor<three, 't>(graph, name, head, output, [|number<'dim0>.Val; number<'dim1>.Val; number<'dim2>.Val|])
        interface ITensor3
        member x.Dim0 = number<'dim0>
        member x.Dim1 = number<'dim1>
        member x.Dim2 = number<'dim2>

    /// 4-D Tensor
    and Tensor<'dim0, 'dim1, 'dim2, 'dim3, 't when 'dim0 :> Number and 'dim1 :> Number and 'dim2 :> Number and 'dim3 :> Number and 't:> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable>(graph:IGraph, name:string, head:Node, output:int) =
        inherit Tensor<four, 't>(graph, name, head, output, [|number<'dim0>.Val; number<'dim1>.Val; number<'dim2>.Val; number<'dim3>.Val|])
        interface ITensor4
        member x.Dim0 = number<'dim0>
        member x.Dim1 = number<'dim1>
        member x.Dim2 = number<'dim2>
        member x.Dim3 = number<'dim3>
//and

