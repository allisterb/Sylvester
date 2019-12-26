namespace Sylvester.Tensors

open System.Linq

open TensorFlow

open Sylvester
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Collections
open Sylvester.Graphs
open Sylvester.Tensors
open Sylvester.tf

type Placeholder(graph:IGraph) =
    inherit Api()
    
    let tfGraph = new TF_Graph(graph.Handle)
    interface IUnknownShape with  
        member val Rank:Option<int> = None with get, set
        member val Dims:Option<int[]> = None with get, set

    
    member x.Shape = x :> IUnknownShape

    member x.SetRank(r:int) = x.Shape.Rank <- Some r
    
    member x.SetDims(dims:int[]) = x.Shape.Dims <- Some dims

type Placeholder<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1 when 'd10 :> Base10Digit and 'd9 :> Base10Digit 
and 'd8 :> Base10Digit and 'd7 :> Base10Digit and 'd6 :> Base10Digit
and 'd5 :> Base10Digit and 'd4 :> Base10Digit and 'd3 :> Base10Digit and 'd2 :> Base10Digit 
and 'd1 :> Base10Digit>(graph:IGraph) =
    inherit Placeholder(graph)

    interface IPartialShape<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1> with
        member x.Rank = N10<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>()
