namespace Sylvester.tf

open System

open TensorFlow

open Sylvester
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Tensors
open Sylvester.Graphs

[<AbstractClass>]
type Tensor(graph: IGraph, dt: TF_DataType, name:string, ?shape:int64[]) = 
    inherit Api()
    
    member x.TFGraph = new TF_Graph(graph.Handle)

    member x.Type = dt

    member x._Type = Convert.ToInt64(int x.Type)
    
    member x.Name = name

    interface IUnknownShape with  
        member val Rank:Option<int> = if shape.IsSome then Some shape.Value.Length else None  with get, set
        member val Dims:Option<int64[]> = shape with get, set        

    member x.Shape = x :> IUnknownShape
    
    member x.SetRank(r:int) = x.Shape.Rank <- Some r
        
    member x.SetDims(dims:int64[]) = x.Shape.Dims <- Some dims

    abstract member TFOutput:TF_Output
    