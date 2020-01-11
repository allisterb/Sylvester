namespace Sylvester.tf

open System

open Sylvester
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Collections
open Sylvester.Graphs
open Sylvester.Tensors

[<AutoOpen>]
module Matrix =
    [<StructuredFormatDisplay("{Display}")>]
    type Matrix<'dim0, 'dim1, 't when 'dim0 :> Number and 'dim1 :> Number and 't : struct and 't:> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>
        internal (graph:ITensorGraph, head:Node, output:int) =
        inherit Tensor<Rank.two, 't>(graph, head, output, [|number<'dim0>.Val; number<'dim1>.Val|])
        interface IMatrix
        member x.Dim0 = number<'dim0>
        member x.Dim1 = number<'dim1>
        member x.Display = sprintf "Matrix<%i, %i, %s>" x.Dim0.IntVal x.Dim1.IntVal (dataType<'t>.ToString())
        
        internal new(name:string, ?graph:ITensorGraph) = 
            let g = defaultArg graph defaultGraph
            let shape = [|number<'dim0>.Val; number<'dim1>.Val|]
            let op = tf(g).Placeholder(dataType<'t>, shape, name)
            new Matrix<'dim0, 'dim1, 't>(g, new Node(g, op, []), 0)

        static member (*) (l:Matrix<'a, 'dim1, 't>, r:Matrix<'dim1, 'b, 't>) = 
            let node = matmul l.Head r.Head
            node.Inputs <- [l; r]
            createEdge<Matrix<'a, 'b, 't>>(l.TensorGraph, node, 0)