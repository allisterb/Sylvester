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

[<AutoOpen>]
module Tensors = 
    /// Tensor of unknown rank and dimensions
    type Tensor<'t when 't:> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable>(graph:ITensorGraph, name:string, head:Node, output:int, ?shape:int64[]) = 
        inherit Edge(graph, name, head, output, dataType<'t>, defaultArg shape null)

        new(name:string, ?shape:int64[]) = 
            let g = defaultGraph
            new Tensor<'t>(g, name, new Node(g, "Placeholder", tf(g).Placeholder(dataType<'t>, defaultArg shape null), []), 0, defaultArg shape null) 
    
    
    /// Tensor of known rank but unknown dimensions
    and Tensor<'r, 't when 'r :> Number and 't:> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable>(graph:ITensorGraph, name:string, head:Node, output:int, ?shape:int64[]) = 
        inherit Edge<'r>(graph, name, head, output, dataType<'t>, defaultArg shape (Array.create number<'r>.IntVal 0L))
        do if number<'r>.IntVal > 0 && shape.IsSome && shape.Value.Length <> number<'r>.IntVal then failwith "The shape array parameter length does not match the tensor's type rank."
        
        new(name:string, ?shape:int64[]) = 
            let g = defaultGraph
            new Tensor<'r, 't>(g, name, new Node(g, "Placeholder", tf(g).Placeholder(dataType<'t>, defaultArg shape (Array.create number<'r>.IntVal 0L)), []), 0, defaultArg shape (Array.create number<'r>.IntVal 0L))
            
        //static member inline (+) (l:Tensor<'r, 't>, r:Tensor<'r, 't> ) :Tensor<'r, 't> = new Tensor<'r, 't>(l.TensorGraph.MakeName("Add"), 
        
        
    /// Vector

    /// Matrix
    and Matrix<'dim0, 'dim1, 't when 'dim0 :> Number and 'dim1 :> Number and 't:> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable>(graph:ITensorGraph, name:string, head:Node, output:int) =
        inherit Tensor<two, 't>(graph, name, head, output, [|number<'dim0>.Val; number<'dim1>.Val|])
        interface IMatrix
        member x.Dim0 = number<'dim0>
        member x.Dim1 = number<'dim1>

        new(name:string) = 
            let g = defaultGraph
            let shape = [|number<'dim0>.Val; number<'dim1>.Val|]
            new Matrix<'dim0, 'dim1, 't>(g, name, new Node(g, "Placeholder", tf(g).Placeholder(dataType<'t>, shape), []), 0)

    /// 3-D Tensor
    and Tensor<'dim0, 'dim1, 'dim2, 't when 'dim0 :> Number and 'dim1 :> Number and 'dim2 :> Number and 't:> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable>(graph:ITensorGraph, name:string, head:Node, output:int) =
        inherit Tensor<three, 't>(graph, name, head, output, [|number<'dim0>.Val; number<'dim1>.Val; number<'dim2>.Val|])
        interface ITensor3
        member x.Dim0 = number<'dim0>
        member x.Dim1 = number<'dim1>
        member x.Dim2 = number<'dim2>

        new(name:string) = 
            let g = defaultGraph
            let shape = [|number<'dim0>.Val; number<'dim1>.Val; number<'dim2>.Val|]
            new Tensor<'dim0, 'dim1, 'dim2, 't>(g, name, new Node(g, "Placeholder", tf(g).Placeholder(dataType<'t>, shape), []), 0)

    /// 4-D Tensor
    and Tensor<'dim0, 'dim1, 'dim2, 'dim3, 't when 'dim0 :> Number and 'dim1 :> Number and 'dim2 :> Number and 'dim3 :> Number and 't:> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable>(graph:ITensorGraph, name:string, head:Node, output:int) =
        inherit Tensor<four, 't>(graph, name, head, output, [|number<'dim0>.Val; number<'dim1>.Val; number<'dim2>.Val; number<'dim3>.Val|])
        interface ITensor4
        member x.Dim0 = number<'dim0>
        member x.Dim1 = number<'dim1>
        member x.Dim2 = number<'dim2>
        member x.Dim3 = number<'dim3>

        new(name:string) = 
            let g = defaultGraph
            let shape = [|number<'dim0>.Val; number<'dim1>.Val; number<'dim2>.Val; number<'dim3>.Val|]
            new Tensor<'dim0, 'dim1, 'dim2, 'dim3, 't>(g, name, new Node(g, "Placeholder", tf(g).Placeholder(dataType<'t>, shape), []), 0)

    /// 5-D Tensor
    and Tensor<'dim0, 'dim1, 'dim2, 'dim3, 'dim4, 't when 'dim0 :> Number and 'dim1 :> Number and 'dim2 :> Number and 'dim3 :> Number and 'dim4 :> Number and 't:> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable>(graph:ITensorGraph, name:string, head:Node, output:int) =
        inherit Tensor<five, 't>(graph, name, head, output, [|number<'dim0>.Val; number<'dim1>.Val; number<'dim2>.Val; number<'dim3>.Val; number<'dim4>.Val|])
        interface ITensor5
        member x.Dim0 = number<'dim0>
        member x.Dim1 = number<'dim1>
        member x.Dim2 = number<'dim2>
        member x.Dim3 = number<'dim3>
        member x.Dim4 = number<'dim4>

        new(name:string) = 
            let g = defaultGraph
            let shape = [|number<'dim0>.Val; number<'dim1>.Val; number<'dim2>.Val; number<'dim3>.Val; number<'dim4>.Val|]
            new Tensor<'dim0, 'dim1, 'dim2, 'dim3, 'dim4, 't>(g, name, new Node(g, "Placeholder", tf(g).Placeholder(dataType<'t>, shape), []), 0)

    /// 6-D Tensor
    and Tensor<'dim0, 'dim1, 'dim2, 'dim3, 'dim4, 'dim5, 't when 'dim0 :> Number and 'dim1 :> Number and 'dim2 :> Number and 'dim3 :> Number and 'dim4 :> Number and 'dim5 :> Number and 't:> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable>(graph:ITensorGraph, name:string, head:Node, output:int) =
        inherit Tensor<six, 't>(graph, name, head, output, [|number<'dim0>.Val; number<'dim1>.Val; number<'dim2>.Val; number<'dim3>.Val; number<'dim4>.Val; number<'dim5>.Val|])
        interface ITensor6
        member x.Dim0 = number<'dim0>
        member x.Dim1 = number<'dim1>
        member x.Dim2 = number<'dim2>
        member x.Dim3 = number<'dim3>
        member x.Dim4 = number<'dim4>
        member x.Dim5 = number<'dim5>

        new(name:string) = 
            let g = defaultGraph
            let shape = [|number<'dim0>.Val; number<'dim1>.Val; number<'dim2>.Val; number<'dim3>.Val; number<'dim4>.Val; number<'dim5>.Val|]
            new Tensor<'dim0, 'dim1, 'dim2, 'dim3, 'dim4, 'dim5,'t>(g, name, new Node(g, "Placeholder", tf(g).Placeholder(dataType<'t>, shape), []), 0)

    /// 7-D Tensor
    and Tensor<'dim0, 'dim1, 'dim2, 'dim3, 'dim4, 'dim5, 'dim6, 't when 'dim0 :> Number and 'dim1 :> Number and 'dim2 :> Number and 'dim3 :> Number and 'dim4 :> Number and 'dim5 :> Number and 'dim6 :> Number and 't:> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable>(graph:ITensorGraph, name:string, head:Node, output:int) =
        inherit Tensor<seven, 't>(graph, name, head, output, [|number<'dim0>.Val; number<'dim1>.Val; number<'dim2>.Val; number<'dim3>.Val; number<'dim4>.Val; number<'dim5>.Val; number<'dim6>.Val|])
        interface ITensor7
        member x.Dim0 = number<'dim0>
        member x.Dim1 = number<'dim1>
        member x.Dim2 = number<'dim2>
        member x.Dim3 = number<'dim3>
        member x.Dim4 = number<'dim4>
        member x.Dim5 = number<'dim5>
        member x.Dim6 = number<'dim6>

        new(name:string) = 
            let g = defaultGraph
            let shape = [|number<'dim0>.Val; number<'dim1>.Val; number<'dim2>.Val; number<'dim3>.Val; number<'dim4>.Val; number<'dim5>.Val; number<'dim6>.Val|]
            new Tensor<'dim0, 'dim1, 'dim2, 'dim3, 'dim4, 'dim5,'dim6, 't>(g, name, new Node(g, "Placeholder", tf(g).Placeholder(dataType<'t>, shape), []), 0)

    /// 8-D Tensor
    and Tensor<'dim0, 'dim1, 'dim2, 'dim3, 'dim4, 'dim5, 'dim6, 'dim7, 't when 'dim0 :> Number and 'dim1 :> Number and 'dim2 :> Number and 'dim3 :> Number and 'dim4 :> Number and 'dim5 :> Number and 'dim6 :> Number and 'dim7 :> Number and 't:> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable>(graph:ITensorGraph, name:string, head:Node, output:int) =
        inherit Tensor<eight, 't>(graph, name, head, output, [|number<'dim0>.Val; number<'dim1>.Val; number<'dim2>.Val; number<'dim3>.Val; number<'dim4>.Val; number<'dim5>.Val; number<'dim6>.Val; number<'dim7>.Val|])
        interface ITensor8
        member x.Dim0 = number<'dim0>
        member x.Dim1 = number<'dim1>
        member x.Dim2 = number<'dim2>
        member x.Dim3 = number<'dim3>
        member x.Dim4 = number<'dim4>
        member x.Dim5 = number<'dim5>
        member x.Dim6 = number<'dim6>
        member x.Dim7 = number<'dim7>

        new(name:string) = 
            let g = defaultGraph
            let shape = [|number<'dim0>.Val; number<'dim1>.Val; number<'dim2>.Val; number<'dim3>.Val; number<'dim4>.Val; number<'dim5>.Val; number<'dim6>.Val; number<'dim7>.Val|]
            new Tensor<'dim0, 'dim1, 'dim2, 'dim3, 'dim4, 'dim5,'dim6, 'dim7, 't>(g, name, new Node(g, "Placeholder", tf(g).Placeholder(dataType<'t>, shape), []), 0)

    /// 9-D Tensor
    and Tensor<'dim0, 'dim1, 'dim2, 'dim3, 'dim4, 'dim5, 'dim6, 'dim7, 'dim8, 't when 'dim0 :> Number and 'dim1 :> Number and 'dim2 :> Number and 'dim3 :> Number and 'dim4 :> Number and 'dim5 :> Number and 'dim6 :> Number and 'dim7 :> Number and 'dim8 :> Number and 't:> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable>(graph:ITensorGraph, name:string, head:Node, output:int) =
        inherit Tensor<nine, 't>(graph, name, head, output, [|number<'dim0>.Val; number<'dim1>.Val; number<'dim2>.Val; number<'dim3>.Val; number<'dim4>.Val; number<'dim5>.Val; number<'dim6>.Val; number<'dim7>.Val; number<'dim8>.Val|])
        interface ITensor9
        member x.Dim0 = number<'dim0>
        member x.Dim1 = number<'dim1>
        member x.Dim2 = number<'dim2>
        member x.Dim3 = number<'dim3>
        member x.Dim4 = number<'dim4>
        member x.Dim5 = number<'dim5>
        member x.Dim6 = number<'dim6>
        member x.Dim7 = number<'dim7>

        new(name:string) = 
            let g = defaultGraph
            let shape = [|number<'dim0>.Val; number<'dim1>.Val; number<'dim2>.Val; number<'dim3>.Val; number<'dim4>.Val; number<'dim5>.Val; number<'dim6>.Val; number<'dim7>.Val; number<'dim8>.Val|]
            new Tensor<'dim0, 'dim1, 'dim2, 'dim3, 'dim4, 'dim5,'dim6, 'dim7, 'dim8, 't>(g, name, new Node(g, "Placeholder", tf(g).Placeholder(dataType<'t>, shape), []), 0)


    type Mat<'dim0, 'dim1, 't when 'dim0 :> Number and 'dim1 :> Number and 't:> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable> = Matrix<'dim0, 'dim1, 't>

    type Mat<'dim0, 'dim1 when 'dim0 :> Number and 'dim1 :> Number> = Mat<'dim0, 'dim1, float32>