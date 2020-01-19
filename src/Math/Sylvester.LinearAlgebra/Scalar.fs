namespace Sylvester.LinearAlgebra

open System

open Sylvester.Arithmetic
open Sylvester.Tensors

[<StructuredFormatDisplay("<Scalar>")>]
type Scalar<'t when 't: struct and 't:> ValueType and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>
    (ops:ILinearAlgebraOps, value:'t) = 
    interface IScalar
        
    new(name:string, ?graph:ITensorGraph) = 
        let g = defaultArg graph defaultGraph
        let shape = [|0L|]
        let op = tf(g).Placeholder(dataType<'t>, shape, name)
        new Scalar<'t>(g, new Node(g, op, []), 0)