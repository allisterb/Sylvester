namespace Sylvester.tf

open System

open Sylvester.Arithmetic

[<StructuredFormatDisplay("{Display}")>]
type Matrix<'dim0, 'dim1, 't when 'dim0 :> Number and 'dim1 :> Number and 't : struct and 't:> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>
    (graph:ITensorGraph, head:Node, output:int) =
    inherit Tensor<Rank.two, 't>(graph, head, output, [|number<'dim0>.Val; number<'dim1>.Val|])
    interface IMatrix
    member x.Dim0 = number<'dim0>
    member x.Dim1 = number<'dim1>
    member x.Display = sprintf "Matrix<%i, %i, %s>" x.Dim0.IntVal x.Dim1.IntVal (dataType<'t>.ToString())
        
    new(name:string, ?graph:ITensorGraph) = 
        let g = defaultArg graph defaultGraph
        let shape = [|number<'dim0>.Val; number<'dim1>.Val|]
        let op = tf(g).Placeholder(dataType<'t>, shape, name)
        new Matrix<'dim0, 'dim1, 't>(g, new Node(g, op, []), 0)

    static member (*) (l:Matrix<'a, 'b, 't>, r: Matrix<'b, 'c, 't>) = 
        let node = matmul l.Head r.Head
        node.Inputs <- [l; r]
        createEdge<Matrix<'a, 'c, 't>>(l.Graph, node, 0)

type Mat<'dim0, 'dim1 when 'dim0 :> Number and 'dim1 :> Number> = Matrix<'dim0, 'dim1, float32>
type MatF<'dim0, 'dim1 when 'dim0 :> Number and 'dim1 :> Number> = Matrix<'dim0, 'dim1, float>
    
type MatN<'dim0, 'dim1 when 'dim0 :> Number and 'dim1 :> Number> = Matrix<'dim0, 'dim1, int>
type MatB<'dim0, 'dim1 when 'dim0 :> Number and 'dim1 :> Number> = Matrix<'dim0, 'dim1, byte>
    
type MatL<'dim0, 'dim1 when 'dim0 :> Number and 'dim1 :> Number> = Matrix<'dim0, 'dim1, int64>
type MatU<'dim0, 'dim1 when 'dim0 :> Number and 'dim1 :> Number> = Matrix<'dim0, 'dim1, uint64>
    
    