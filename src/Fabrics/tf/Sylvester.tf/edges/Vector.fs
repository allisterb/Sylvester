namespace Sylvester.tf

open System

open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Tensors

[<AutoOpen>]
module Vector =
    
    [<StructuredFormatDisplay("{Display}")>]
    type Vector<'dim0, 't when 'dim0 :> Number and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>
        internal (graph:ITensorGraph, head:Node, output:int) =
        inherit Tensor<Rank.one, 't>(graph, head, output, [|number<'dim0>.Val|])
        interface IVector
        member x.Dim0:'dim0 = number<'dim0>
        member x.Display = sprintf "Vector<%i, %s>" (x.Dim0.IntVal) (dataType<'t>.ToString())
        
        internal new(name:string, ?graph:ITensorGraph) = 
            let g = defaultArg graph defaultGraph
            let shape = [|number<'dim0>.Val|]
            let op = tf(g).Placeholder(dataType<'t>, shape, name) 
            new Vector<'dim0, 't>(g, new Node(g, op, []), 0)