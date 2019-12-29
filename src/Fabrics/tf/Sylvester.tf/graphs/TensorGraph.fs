namespace Sylvester.tf

open System
open System.Collections.Generic;
open System.Runtime.CompilerServices

open TensorFlow

open Sylvester
open Sylvester.Arithmetic
open Sylvester.Collections
open Sylvester.Graphs
open Sylvester.Tensors

/// A graph of tensor operations.
type TensorGraph<'input, 'output when 'input :> Number and 'output :> Number>(scope:string) = 
    inherit Graph<'input, 'output, Edge>(scope)
    
    let tfGraph = c_api.TF_NewGraph() |?? lazy failwith "Could not create new TF_Graph."
    
    do tfGraph.SetNameScope(scope)

    do base.Initialized <- tfGraph <> null && tfGraph.NameScope = scope
        
    member internal x._Graph = tfGraph

    member x.NameScope with get() = tfGraph.NameScope

    member x.NewSubNameScope(subName:string) = 
        if empty subName then failwith "New sub-scope name cannot be empty." else x.NameScope + "/" + subName

    member x.WithOpName(opName:string) = tfGraph.MakeUniqueName(opName) 
    
    interface IGraph with
        member x.NameScope = scope
        member x.Handle = tfGraph.__Instance
        member x.GetName s = tfGraph.GetName s

    member val Status = {Code = TF_Code.TF_UNKNOWN; Message = ""} with get, set
    
    member x.UpdateStatus(status:TF_Status) = 
          x.Status <- {Code = tf_status.TF_GetCode(status); Message = tf_status.TF_Message(status)}
          do if x.Status.Code <> TF_Code.TF_OK then failwith "An operation in this scope did not return TF_OK."

    member val Inputs = vanew<'input, Edge> with get,set 

    member val Outputs = vanew<'input, Edge> with get,set

    member x.Nodes = new Dictionary<string, Node>()
        
    member x.Edges = new Dictionary<string, Edge>()

    member x.AddEdge(e:Edge) =
        if x.Edges.ContainsKey(e.Name) then
            failwithf "The edge with name %s already exists in this graph." e.Name
        else
            x.Edges.Add(e.Name, e)
            if not <| x.Nodes.ContainsKey(e.Head.Name) then x.AddNode(e.Head)
            e

    member x.AddNode(n:Node) =
        if x.Nodes.ContainsKey(n.Name) then
            failwithf "The node with name %s already exists in this graph." n.Name
        else
            x.Nodes.Add(n.Name, n)
            Seq.iter (fun (e:Edge) -> if not <| x.Edges.ContainsKey(e.Name) then x.Edges.Add(e.Name, e)) n.Inputs
            ()
                   
    new() = TensorGraph("")

    new(scope:string, inputs: VArray<'input, Input>) as graph = 
        new TensorGraph<'input, 'output>(scope) then
            graph.Inputs <- inputs.Map (fun i -> Edge(graph, i.Name, new Node(graph, "Placeholder", graph._Graph.Placeholder(i.DataType), []), 0, i.DataType)) 
            
and GraphStatus = {Code: TF_Code; Message: string}

/// A tensor graph node consists of an operation with input and edges
and Node(graph: IGraph, name:string, op:TF_Output[], inputs: Edge list) = 
    inherit Api()
    
    member x.Graph = graph

    member x._Graph = graph

    member x.Name = x.Graph.GetName(name)

    member x.Op = op

    member x.Inputs = inputs

    new(graph: IGraph, name:string, op:TF_Output, inputs: Edge list) = Node(graph, name, [|op|], inputs)

/// A tensor graph edge represents tensor data of known or unknown shape flowing into or out of a graph and between graph nodes.
and Edge(graph: IGraph, name:string, head:Node, output:int, dt:TF_DataType, ?shape:int64[]) = 
    inherit Api()
    
    member x.Graph = graph

    member x._Graph = graph

    member x.DataType = dt

    member x._Type = Convert.ToInt64(int x.DataType)
    
    member x.Name = name

    interface IUnknownShape with  
        member val Rank:Option<int> = if shape.IsSome then Some shape.Value.Length else None  with get, set
        member val Dims:Option<int64[]> = shape with get, set

    interface IEdge with
        member x.Graph = graph
        member x.Name = name
        member x._DataType = Convert.ToInt64(int dt)

    member x.Shape = x :> IUnknownShape
    
    member x.Head:Node = head

    member x.Output = x.Head.Op.[output]

/// A tensor graph edge with partially known shape
and Edge<'r when 'r :> Number>(graph:TensorGraph<_,_>, name:string, head: Node, output:int, dt:TF_DataType, shape:int64[]) =
    inherit Edge(graph, name, head, output, dt, shape)

    interface IPartialShape<'r> with
        member x.Rank = number<'r>

and Input = { Name: string; DataType: TF_DataType; Shape: int64[] option }

[<AutoOpen>]
module TensorGraph = 
    let dataType<'t> =
        match typeof<'t>.Name with
        | "Boolean" -> TF_DataType.TF_BOOL;
        | "SByte" -> TF_DataType.TF_INT8;
        | "Byte" -> TF_DataType.TF_UINT8;
        | "Int16" -> TF_DataType.TF_INT16;
        | "UInt16"-> TF_DataType.TF_UINT16;
        | "Int32" -> TF_DataType.TF_INT32;
        | "UInt32" -> TF_DataType.TF_UINT32;
        | "Int64" -> TF_DataType.TF_INT64;
        | "UInt64" -> TF_DataType.TF_UINT64;
        | "Single" -> TF_DataType.TF_FLOAT;
        | "Double" -> TF_DataType.TF_DOUBLE;
        | "Complex" -> TF_DataType.TF_COMPLEX128;
        | _ -> failwithf "The type %s cannot be converted to a TensorFlow tensor type" typeof<'t>.Name

