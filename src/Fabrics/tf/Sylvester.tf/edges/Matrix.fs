﻿namespace Sylvester.tf

open System

open Sylvester
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Collections
open Sylvester.Graphs
open Sylvester.Tensors

[<AutoOpen>]
module Matrix =
    type Matrix<'dim0, 'dim1, 't when 'dim0 :> Number and 'dim1 :> Number and 't : struct and 't:> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>(graph:ITensorGraph, name:string, head:Node, output:int) =
        inherit Tensor<two, 't>(graph, name, head, output, [|number<'dim0>.Val; number<'dim1>.Val|])
        interface IMatrix
        member x.Dim0 = number<'dim0>
        member x.Dim1 = number<'dim1>

        new(name:string) = 
            let g = defaultGraph
            let shape = [|number<'dim0>.Val; number<'dim1>.Val|]
            new Matrix<'dim0, 'dim1, 't>(g, name, new Node(g, name, tf(g).Placeholder(dataType<'t>, shape, name), []), 0)

        static member (*) (l:'x, r:'y when 'x :> Matrix<'dim0, 'dim1, 't> and 'y :> Matrix<'dim1, 'dim0, 't>) = 
            let node = matmul l.Head r.Head
            node.Inputs <- [l; r]
            Activator.CreateInstance (typeof<'x>, ([|defaultGraph :> obj; node.Name :> obj; node :> obj; 0 :> obj|])) :?> 'x

    
    type Mat<'dim0, 'dim1, 't when 'dim0 :> Number and 'dim1 :> Number and 't:> ValueType and 't : struct  and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable> = Matrix<'dim0, 'dim1, 't>

    type Mat<'dim0, 'dim1 when 'dim0 :> Number and 'dim1 :> Number> = Mat<'dim0, 'dim1, float32>