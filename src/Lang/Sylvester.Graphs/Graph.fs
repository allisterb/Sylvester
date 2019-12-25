namespace Sylvester.Graphs

open Sylvester
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Collections
open Sylvester.Tensors

[<AbstractClass>]
type Graph<'a, 'b, 'c, 'd when 'a :> Base10Digit and 'b :> Base10Digit and 'c:> Base10Digit and 'd:> Base10Digit>(scope:string) = 
    inherit Api()
    
    member x.NumInputs = getN<N2<'a, 'b>>

    member x.NumOutputs = getN<N2<'c, 'd>>

    abstract member Inputs:VArray<'a, 'b, IGraphInput> with get
    
    abstract member Outputs:VArray<'a, 'b, IGraphOutput> with get

    interface IGraph with
        member x.NameScope = scope
        member x.NumInputs = x.NumInputs.IntVal
        member x.NumOutputs = x.NumOutputs.IntVal
        member x.Inputs = x.Inputs._Array
        member x.Outputs = x.Outputs._Array

and IGraph =
    abstract member NameScope:string with get
    abstract member NumInputs:int
    abstract member NumOutputs:int
    abstract member Inputs:array<IGraphInput>
    abstract member Outputs:array<IGraphOutput>

and IGraphInput = 
    inherit IUnknownShape
    abstract member Graph:IGraph
    abstract member Name:string

and IGraphOutput = 
    inherit IUnknownShape
    abstract member Graph:IGraph
    abstract member Name:string


    
    