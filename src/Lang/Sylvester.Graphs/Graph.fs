namespace Sylvester.Graphs

open Sylvester
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Collections
open Sylvester.Tensors

[<AbstractClass>]
type Graph<'input, 'output, 'edge when 'input :> Number and 'output :> Number and 'edge :> IEdge>(scope:string) = 
    inherit Api()
    
    member inline x.NumInputs = number<'input>

    member inline x.NumOutputs = number<'output>

    member inline x.Inputs:VArray<'input, 'edge> = VArray<'input, 'edge>()
    
    member inline x.Outputs:VArray<'output, 'edge> = VArray<'output, 'edge>()

and IGraph = 
    abstract member NameScope:string
    abstract member MakeName:string->string
    abstract member GetName:string->string
    abstract member Handle:nativeint

and INode<'n> = 
    abstract member Graph:IGraph with get,set
    abstract member Name:string
    abstract member Output:'n

and IEdge = 
    inherit IUnknownShape
    abstract member Graph:IGraph with get,set
    abstract member Name:string
    abstract member _DataType:int64

and IEdge<'n when 'n :> Number> = 
        inherit IEdge
        inherit IPartialShape<'n>    



