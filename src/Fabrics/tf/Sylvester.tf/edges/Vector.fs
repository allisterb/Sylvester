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

type Vector<'dim0, 't when 'dim0 :> Number and 't:> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable>(graph:ITensorGraph, name:string, head:Node, output:int) =
    inherit Tensor<one, 't>(graph, name, head, output, [|number<'dim0>.Val|])
    interface IVector
    member x.Dim0:'dim0 = number<'dim0>

    new(name:string) = 
        let g = TensorGraph<zero, zero>.DefaultGraph
        let shape = [|number<'dim0>.Val|]
        new Vector<'dim0, 't>(g, name, new Node(g, "Placeholder", ops(g).Placeholder(dataType<'t>, shape), []), 0)
