namespace Sylvester.Graphs

open Sylvester
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Collections
open Sylvester.Tensors

[<AbstractClass>]
[<StructuredFormatDisplay("{Display}")>]
type Graph<'input, 'output, 'edge when 'input :> Number and 'output :> Number and 'edge :> IEdge>(scope:string) = 
    inherit Api()
    
    member x.NumInputs = number<'input>

    member x.NumOutputs = number<'output>

    member x.Inputs:VArray<'input, 'edge> = VArray<'input, 'edge>()
    
    member x.Outputs:VArray<'output, 'edge> = VArray<'output, 'edge>()

    member x.Display = sprintf "Graph<%i, %i>" x.NumInputs.IntVal x.NumOutputs.IntVal 

and IGraph = 
    abstract member Handle:nativeint

and INode<'n> = 
    abstract member Graph:IGraph with get,set
    abstract member Name:string
    abstract member Op:'n

and IEdge = 
    inherit IUnknownShape
    abstract member Graph:IGraph with get,set
    abstract member Name:string
    abstract member DataType:int64

and IEdge<'n when 'n :> Number> = 
        inherit IEdge
        inherit IPartialShape<'n>  