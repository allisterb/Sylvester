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
        inherit Edge<zero>(graph, name, head, output, dataType<'t>, Array.Empty<int64>())
        interface IScalar

    /// Vector
    and Vector<'dim0, 't when 'dim0 :> Number and 't:> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable>(graph:IGraph, name:string, head:Node, output:int) =
        inherit Edge<one>(graph, name, head, output, dataType<'t>, [|number<'dim0>.Val|])
        interface IVector
        member x.Dim0:'dim0 = number<'dim0>

    /// Matrix
    and Matrix<'dim0, 'dim1, 't when 'dim0 :> Number and 'dim1 :> Number and 't:> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable>(graph:IGraph, name:string, head:Node, output:int) =
        inherit Edge<two>(graph, name, head, output, dataType<'t>, [|number<'dim0>.Val; number<'dim1>.Val|])
        member x.Dim0 = number<'dim0>
        member x.Dim1 = number<'dim1>

//and

