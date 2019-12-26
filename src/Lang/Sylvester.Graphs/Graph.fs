namespace Sylvester.Graphs

open System
open System.Linq

open Sylvester
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Collections
open Sylvester.Tensors

[<AbstractClass>]
type Graph<'a, 'b, 'c, 'd, 'e, 'f when 'a :> Base10Digit and 'b :> Base10Digit and 'c:> Base10Digit and 'd:> Base10Digit
and 'e :> IGraphInput and 'f :> IGraphOutput>(scope:string) as this = 
    inherit Api()
    
    member x.NumInputs = N2<'a, 'b>()

    member x.NumOutputs = N2<'c, 'd>()

    member val Inputs:VArray<'a, 'b, 'e> = VArray<'a, 'b, 'e>(Array.create ((int) this.NumInputs) (Unchecked.defaultof<'e>)) with get,set
    
    member val Outputs:VArray<'c, 'd, 'f> = VArray<'c, 'd, 'f>(Array.create ((int) this.NumOutputs) (Unchecked.defaultof<'f>)) with get,set

    interface IGraph with
        member x.NameScope = scope
        member val Handle = IntPtr.Zero with get 
        member x.NumInputs = x.NumInputs.IntVal
        member x.NumOutputs = x.NumOutputs.IntVal
        member x.Inputs = x.Inputs._Array.Cast<IGraphInput>().ToArray()
        member x.Outputs = x.Outputs._Array.Cast<IGraphOutput>().ToArray()

and IGraph =
    abstract member Handle:nativeint with get
    abstract member NameScope:string with get
    abstract member NumInputs:int
    abstract member NumOutputs:int
    abstract member Inputs:array<IGraphInput>
    abstract member Outputs:array<IGraphOutput>

and IGraphInput = 
    inherit IUnknownShape
    abstract member Graph:IGraph
    abstract member Name:string
    abstract member _Type:int64

and IGraphInput<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1 when 'd10 :> Base10Digit and 'd9 :> Base10Digit 
    and 'd8 :> Base10Digit and 'd7 :> Base10Digit and 'd6 :> Base10Digit
    and 'd5 :> Base10Digit and 'd4 :> Base10Digit and 'd3 :> Base10Digit and 'd2 :> Base10Digit 
    and 'd1 :> Base10Digit> = 
        inherit IGraphInput
        inherit IPartialShape<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>

and IGraphOutput = 
    inherit IUnknownShape
    abstract member Graph:IGraph
    abstract member Name:string
    abstract member _Type:int64

and IGraphOutput<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1 when 'd10 :> Base10Digit and 'd9 :> Base10Digit 
and 'd8 :> Base10Digit and 'd7 :> Base10Digit and 'd6 :> Base10Digit
and 'd5 :> Base10Digit and 'd4 :> Base10Digit and 'd3 :> Base10Digit and 'd2 :> Base10Digit 
and 'd1 :> Base10Digit> = 
    inherit IGraphOutput
    inherit IPartialShape<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>


    
    