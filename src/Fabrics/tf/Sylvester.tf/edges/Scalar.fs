namespace Sylvester.tf

open System

[<StructuredFormatDisplay("<Scalar>")>]
type Scalar<'t when 't: struct and 't:> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>
    (graph:ITensorGraph, head:Node, output:int) = 
    inherit Tensor<Rank.zero, 't>(graph, head, output, [|0L|])
    interface IScalar
        
    new(name:string, ?graph:ITensorGraph) = 
        let g = defaultArg graph defaultGraph
        let shape = [|0L|]
        let op = tf(g).Placeholder(dataType<'t>, shape, name)
        new Scalar<'t>(g, new Node(g, op, []), 0)