namespace Sylvester.tf

open System
open System.Runtime.CompilerServices

open TensorFlow

open Sylvester
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Collections
open Sylvester.Graphs
open Sylvester.Tensors

/// Represents a graph of tensor operations.
type TensorGraph<'a, 'b, 'c, 'd when 'a :> Base10Digit and 'b :> Base10Digit and 'c :> Base10Digit and 'd :> Base10Digit>(scope:string) = 
    inherit Graph<'a, 'b, 'c, 'd, Edge>(scope)
    
    let tfGraph = c_api.TF_NewGraph() |?? lazy failwith "Could not create new TF_Graph."
    
    do tfGraph.SetNameScope(scope)

    do base.Initialized <- tfGraph <> null && tfGraph.NameScope = scope

    member internal x._Graph = tfGraph

    member x.NameScope with get() = tfGraph.NameScope

    member x.NewSubNameScope(subName:string) = 
        if empty subName then failwith "New sub-scope name cannot be empty." else x.NameScope + "/" + subName

    member x.WithOpName(opName:string) = tfGraph.MakeUniqueName(opName) 
    
    member val Status = {Code = TF_Code.TF_UNKNOWN; Message = ""} with get, set
    
    member x.UpdateStatus(status:TF_Status) = 
          x.Status <- {Code = tf_status.TF_GetCode(status); Message = tf_status.TF_Message(status)}
          do if x.Status.Code <> TF_Code.TF_OK then failwith "An operation in this scope did not return TF_OK."
  
    new ()  = TensorGraph("")

    static member create() = TensorGraph()

    static member create (nameScope:string) = TensorGraph(nameScope)

and GraphStatus = {Code: TF_Code; Message: string}

/// Represents a tensor graph node consisting of an operation with input and output tensors
and Node(graph: TensorGraph<_,_,_,_>, inputs: Edge list, outputs:TF_Output[]) = 
    inherit Api()
    
    member x.Graph = graph

    member x._Graph = graph._Graph

    member x.Outputs = outputs

    new(graph: TensorGraph<_,_,_,_>, inputs: Edge list, output:TF_Output) = Node(graph, inputs, [|output|])

/// Represents tensor data of known or unknown shape flowing into or out of a graph and between graph nodes.
and Edge(graph: TensorGraph<_,_,_,_>, name:string, head:TF_Output, dt:TF_DataType, ?shape:int64[]) = 
    inherit Api()
    
    member x.Graph = graph

    member x._Graph = graph._Graph

    member x.DataType = dt

    member x._Type = Convert.ToInt64(int x.DataType)
    
    member x.Name = name

    interface IUnknownShape with  
        member val Rank:Option<int> = if shape.IsSome then Some shape.Value.Length else None  with get, set
        member val Dims:Option<int64[]> = shape with get, set

    interface IEdge with
        member x.Graph = graph :> IGraph
        member x.Name = name
        member x._DataType = Convert.ToInt64(int dt)

    member x.Shape = x :> IUnknownShape
    

    member x.Head = head
/// Represents tensor data with partialy known shape flowing into or out of a graph or node
type Edge<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1 when 'd10 :> Base10Digit and 'd9 :> Base10Digit 
and 'd8 :> Base10Digit and 'd7 :> Base10Digit and 'd6 :> Base10Digit
and 'd5 :> Base10Digit and 'd4 :> Base10Digit and 'd3 :> Base10Digit and 'd2 :> Base10Digit 
and 'd1 :> Base10Digit>(graph:TensorGraph<_,_,_,_>, name:string, output:TF_Output, dt:TF_DataType, shape:int64[]) =
    inherit Edge(graph, name, output, dt, shape)

    interface IPartialShape<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1> with
        member x.Rank = N10<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>()

/// Alias for TensorGraph with max 9 inputs and 9 outputs.
type TensorGraph<'b, 'd when 'b :> Base10Digit and 'd:> Base10Digit> = TensorGraph<``0``, 'b, ``0``, 'd>  
