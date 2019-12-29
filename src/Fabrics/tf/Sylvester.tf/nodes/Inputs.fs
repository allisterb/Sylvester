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
module Inputs =
    type TensorGraph<'input, 'output when 'input :> Number and 'output :> Number> with
        member graph.Input<'t when 't:> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable>(name:string) = 
            new Tensor<'t>(graph, name, new Node(graph, "Placeholder", graph._Graph.Placeholder(dataType<'t>), []), 0)

        member graph.Input<'r, 't when 'r :> Number and 't:> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable>(name:string) = 
            let shape = Array.create number<'r>.IntVal 0L
            new Tensor<'r, 't>(graph, name, new Node(graph, "Placeholder", graph._Graph.Placeholder(dataType<'t>, shape), []), 0, shape)

        member graph.ScalarInput<'t when 't:> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable>(name:string) = 
            let shape = Array.empty<int64>
            new Scalar<'t>(graph, name, new Node(graph, "Placeholder", graph._Graph.Placeholder(dataType<'t>, shape), []), 0)

        member graph.MatrixInput<'dim0, 'dim1, 't when 'dim0 :> Number and 'dim1 :> Number and 't:> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable>(name:string) = 
            let shape = [|number<'dim0>.Val; number<'dim1>.Val; |]
            new Matrix<'dim0, 'dim1, 't>(graph, name, new Node(graph, "Placeholder", graph._Graph.Placeholder(dataType<'t>, shape), []), 0)
       
            