namespace Sylvester.tf

open System
open System.Runtime.CompilerServices

open TensorFlow

open Sylvester
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Collections
open Sylvester.Tensors


type Graph<'a, 'b, 'c, 'd when 'a :> Base10Digit and 'b :> Base10Digit and 'c :> Base10Digit and 'd :> Base10Digit>(scope:string) = 
    inherit Graphs.Graph<'a, 'b, 'c, 'd, Input, Output>(scope)
    
    let tfGraph = c_api.TF_NewGraph() |?? lazy failwith "Could not create new TF_Graph."
    
    do tfGraph.SetNameScope(scope)

    do base.Initialized <- tfGraph <> null

    member internal x._Graph = tfGraph

    member x.NameScope with get() = tfGraph.NameScope

    member x.NewSubNameScope(subName:string) = 
        if empty subName then failwith "New sub-scope name cannot be empty." else x.NameScope + "/" + subName

    member x.WithOpName(opName:string) = tfGraph.MakeUniqueName(opName) 
    
    member val Status = {Code = TF_Code.TF_UNKNOWN; Message = ""} with get, set
    
    member x.UpdateStatus(status:TF_Status) = 
          x.Status <- {Code = tf_status.TF_GetCode(status); Message = tf_status.TF_Message(status)}
          do if x.Status.Code <> TF_Code.TF_OK then failwith "An operation in this scope did not return TF_OK."
  
    new () = Graph("")

    static member create() = Graph()

    static member create(nameScope:string) = Graph(nameScope)

and GraphStatus = {Code: TF_Code; Message: string}

type Graph<'b, 'd when 'b :> Base10Digit and 'd:> Base10Digit> = Graph<``0``, 'b, ``0``, 'd>  
 