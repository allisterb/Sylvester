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
module NDTensor =

    /// 3-D Tensor
    type Tensor<'dim0, 'dim1, 'dim2, 't when 'dim0 :> Number and 'dim1 :> Number and 'dim2 :> Number and 't:> ValueType and 't : struct  and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>(graph:ITensorGraph, name:string, head:Node, output:int) =
        inherit Tensor<three, 't>(graph, name, head, output, [|number<'dim0>.Val; number<'dim1>.Val; number<'dim2>.Val|])
        interface ITensor3
        member x.Dim0 = number<'dim0>
        member x.Dim1 = number<'dim1>
        member x.Dim2 = number<'dim2>

        new(name:string) = 
            let g = defaultGraph
            let shape = [|number<'dim0>.Val; number<'dim1>.Val; number<'dim2>.Val|]
            new Tensor<'dim0, 'dim1, 'dim2, 't>(g, name, new Node(g, name, tf(g).Placeholder(dataType<'t>, shape, name), []), 0)

    /// 4-D Tensor
    type Tensor<'dim0, 'dim1, 'dim2, 'dim3, 't when 'dim0 :> Number and 'dim1 :> Number and 'dim2 :> Number and 'dim3 :> Number and 't:> ValueType and 't : struct  and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>(graph:ITensorGraph, name:string, head:Node, output:int) =
        inherit Tensor<four, 't>(graph, name, head, output, [|number<'dim0>.Val; number<'dim1>.Val; number<'dim2>.Val; number<'dim3>.Val|])
        interface ITensor4
        member x.Dim0 = number<'dim0>
        member x.Dim1 = number<'dim1>
        member x.Dim2 = number<'dim2>
        member x.Dim3 = number<'dim3>

        new(name:string) = 
            let g = defaultGraph
            let shape = [|number<'dim0>.Val; number<'dim1>.Val; number<'dim2>.Val; number<'dim3>.Val|]
            new Tensor<'dim0, 'dim1, 'dim2, 'dim3, 't>(g, name, new Node(g, name, tf(g).Placeholder(dataType<'t>, shape, name), []), 0)

    /// 5-D Tensor
    type Tensor<'dim0, 'dim1, 'dim2, 'dim3, 'dim4, 't when 'dim0 :> Number and 'dim1 :> Number and 'dim2 :> Number and 'dim3 :> Number and 'dim4 :> Number and 't:> ValueType and 't : struct  and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>(graph:ITensorGraph, name:string, head:Node, output:int) =
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
            new Tensor<'dim0, 'dim1, 'dim2, 'dim3, 'dim4, 't>(g, name, new Node(g, name, tf(g).Placeholder(dataType<'t>, shape, name), []), 0)

    /// 6-D Tensor
    type Tensor<'dim0, 'dim1, 'dim2, 'dim3, 'dim4, 'dim5, 't when 'dim0 :> Number and 'dim1 :> Number and 'dim2 :> Number and 'dim3 :> Number and 'dim4 :> Number and 'dim5 :> Number and 't:> ValueType and 't : struct  and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>(graph:ITensorGraph, name:string, head:Node, output:int) =
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
            new Tensor<'dim0, 'dim1, 'dim2, 'dim3, 'dim4, 'dim5,'t>(g, name, new Node(g, name, tf(g).Placeholder(dataType<'t>, shape, name), []), 0)

    /// 7-D Tensor
    type Tensor<'dim0, 'dim1, 'dim2, 'dim3, 'dim4, 'dim5, 'dim6, 't when 'dim0 :> Number and 'dim1 :> Number and 'dim2 :> Number and 'dim3 :> Number and 'dim4 :> Number and 'dim5 :> Number and 'dim6 :> Number and 't:> ValueType and 't : struct  and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>(graph:ITensorGraph, name:string, head:Node, output:int) =
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
            new Tensor<'dim0, 'dim1, 'dim2, 'dim3, 'dim4, 'dim5,'dim6, 't>(g, name, new Node(g, name, tf(g).Placeholder(dataType<'t>, shape, name), []), 0)

    /// 8-D Tensor
    type Tensor<'dim0, 'dim1, 'dim2, 'dim3, 'dim4, 'dim5, 'dim6, 'dim7, 't when 'dim0 :> Number and 'dim1 :> Number and 'dim2 :> Number and 'dim3 :> Number and 'dim4 :> Number and 'dim5 :> Number and 'dim6 :> Number and 'dim7 :> Number and 't:> ValueType and 't : struct  and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>(graph:ITensorGraph, name:string, head:Node, output:int) =
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
            new Tensor<'dim0, 'dim1, 'dim2, 'dim3, 'dim4, 'dim5,'dim6, 'dim7, 't>(g, name, new Node(g, name, tf(g).Placeholder(dataType<'t>, shape, name), []), 0)

    /// 9-D Tensor
    type Tensor<'dim0, 'dim1, 'dim2, 'dim3, 'dim4, 'dim5, 'dim6, 'dim7, 'dim8, 't when 'dim0 :> Number and 'dim1 :> Number and 'dim2 :> Number and 'dim3 :> Number and 'dim4 :> Number and 'dim5 :> Number and 'dim6 :> Number and 'dim7 :> Number and 'dim8 :> Number and 't:> ValueType and 't : struct  and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>(graph:ITensorGraph, name:string, head:Node, output:int) =
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
        member x.Dim8 = number<'dim8>

        new(name:string) = 
            let g = defaultGraph
            let shape = [|number<'dim0>.Val; number<'dim1>.Val; number<'dim2>.Val; number<'dim3>.Val; number<'dim4>.Val; number<'dim5>.Val; number<'dim6>.Val; number<'dim7>.Val; number<'dim8>.Val|]
            new Tensor<'dim0, 'dim1, 'dim2, 'dim3, 'dim4, 'dim5,'dim6, 'dim7, 'dim8, 't>(g, name, new Node(g, name, tf(g).Placeholder(dataType<'t>, shape, name), []), 0)
