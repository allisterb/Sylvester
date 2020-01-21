namespace Sylvester.tf

open System
open System.Collections.Generic;
open System.Reflection

open TensorFlow

open Sylvester
open Sylvester.Arithmetic
open Sylvester.Collections

/// A graph of tensor operations.
type ITensorGraph =
    abstract member Handle:nativeint
    abstract member NameScope:string with get,set
    abstract member MakeName:string->string
    abstract member GetName:string->string
    abstract member Ops:ITensorFlowOps
    abstract member Add: Edge -> unit
    abstract member Add: Node -> unit
    abstract member Export:string->unit
    
/// A graph of tensor operations with a known number of inputs and outputs.
and TensorGraph<'input, 'output when 'input :> Number and 'output :> Number>(scope:string) = 
    inherit Api()
    
    let tfGraph = c_api.TF_NewGraph() |?? lazy failwith "Could not create new TF_Graph."
    
    do tfGraph.Dependencies <- Array.empty<TF_Operation>

    do tfGraph.SetNameScope(scope)

    do base.Initialized <- tfGraph <> null && tfGraph.NameScope = scope
        
    /// VArray of graph inputs
    member val Inputs = vanew<'input, Edge> with get,set 

    /// VArray of graph outputs
    member val Outputs = vanew<'output, Edge> with get,set

    member x.NameScope with get() = tfGraph.NameScope and set(value) = tfGraph.SetNameScope(value)
    
    member x.IsEmpty = x.NameScope = "_"
    
    ///Flat map of graph nodes
    member val internal Nodes = new Dictionary<string, Node>() with get
        
    /// Flat map of graph edges
    member val internal Edges = new Dictionary<string, Edge>() with get

    member internal x.NumPlaceHolders = seq {for n in x.Nodes.Values do yield n.Op} |> Seq.concat |> Seq.filter (fun n -> TF_Graph.GetOpType(n) = "Placeholder") |> Seq.length

    static member val EmptyGraph = TensorGraph<zero, zero>("_") :> ITensorGraph with get/// Add an edge (tensor) to the graph
    
    member internal x.AddEdge(e:Edge) =
        if (e.Graph :> ITensorGraph).NameScope <> x.NameScope then failwith "This tensor does not belong to this graph's namescope."
        do if x.Edges.ContainsKey(e.Name) then failwithf "The edge with name %s already exists in this graph." e.Name
        x.Edges.Add(e.Name, e)
        do if not <| x.Nodes.ContainsKey(e.Head.Name) then x.AddNode(e.Head)
                
    /// Add a node (operation) to the graph
    member internal x.AddNode(n:Node) =
        do if (n.Graph :> ITensorGraph).NameScope <> x.NameScope then failwith "This node does not belong to this graph's namescope."
        do if x.Nodes.ContainsKey(n.Name) then failwithf "The node with name %s already exists in this graph." n.Name                
        let numplaceholders = n.Op |> Seq.filter (fun op -> TF_Graph.GetOpType(op) = "Placeholder") |> Seq.length in 
            if numplaceholders > 0 && x.NumPlaceHolders + numplaceholders > x.Inputs.IntLength then failwithf "Cannot add input node %s as this graph already contains %i input(s)." n.Name x.Inputs.IntLength  
        x.Nodes.Add(n.Name, n)
        Seq.iter (fun (e:Edge) -> if not <| x.Edges.ContainsKey(e.Name) then x.AddEdge(e) |> ignore) n.Inputs
                   
    interface ITensorGraph with
        member x.Handle = tfGraph.__Instance
        member x.MakeName s = tfGraph.MakeName s
        member x.GetName s = tfGraph.GetName s
        member x.NameScope with get() = x.NameScope and set(value) = x.NameScope <- value
        member x.Ops = tfGraph :> ITensorFlowOps
        member x.Add n = x.AddNode(n)
        member x.Add e = x.AddEdge(e)
        member x.Export path = tfGraph.Export path
        
    new() = TensorGraph("")
        
and Graph<'input, 'output when 'input :> Number and 'output :> Number> = TensorGraph<'input, 'output>

/// A tensor graph node consists of an operation with input edges
and Node(graph: ITensorGraph, name:string, op:TF_Output[], inputs: Edge list) = 
    
    member val Graph = graph  with get

    member val Name = name with get 

    member val Inputs = inputs with get, set

    member val Op = op with get

    new(graph: ITensorGraph, op:TF_Output, inputs: Edge list) = Node(graph, c_api.TF_OperationName op.Oper, [|op|], inputs)

/// A tensor graph edge represents tensor data of known or unknown shape flowing into or out of a graph and between graph nodes.
/// Each edge has one head node and data type with known or unknown shape.
and Edge(graph: ITensorGraph, head:Node, output:int, dt:TF_DataType, ?shape:int64[]) = 
    
    member val Graph = graph with get

    member val DataType = dt with get
   
    member val Name = head.Name with get

    member val Head:Node = head with get
    
    member x.Output = x.Head.Op.[output]
    
    member x.Shape = x :> IUnknownShape

    interface IUnknownShape with  
        member val Rank:Option<int> = if shape.IsSome then Some shape.Value.Length else None  with get, set
        member val Dims:Option<int64[]> = shape with get, set

/// A tensor graph edge with partially known shape
and Edge<'r when 'r :> Number>(graph:ITensorGraph, head: Node, output:int, dt:TF_DataType, shape:int64[]) =
    inherit Edge(graph, head, output, dt, shape)
    member val Rank = number<'r>
    interface IPartialShape<'r> with member x.Rank = x.Rank 

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

    let ops (x:obj) =
        match x with
        | :? Node as node -> node.Graph.Ops
        | :? Edge as edge -> edge.Graph.Ops
        | :? ITensorGraph as graph -> graph.Ops
        | _ -> failwith "This type is not a tensor graph or graph element."
      

    let tf (x:obj) =
        match x with
        | :? Node as node -> node.Graph.Ops :?> TF_Graph
        | :? Edge as edge -> edge.Graph.Ops :?> TF_Graph
        | :? ITensorGraph as graph -> graph.Ops :?> TF_Graph
        | _ -> failwith "This type is not a TensorFlow graph or element."
      
    let tg<'input, 'output when 'input :> Number and 'output :>Number>(g:ITensorGraph) = g :?> TensorGraph<'input, 'output>

    let createEdge<'x when 'x :> Edge>(graph:ITensorGraph, head: Node, output:int) =
        Activator.CreateInstance(typeof<'x>, BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance, null, [|graph :> obj; head :> obj; output :> obj|], null) :?> 'x 

    let emptyGraph = TensorGraph<zero, zero>.EmptyGraph

    let mutable defaultGraph = emptyGraph    