namespace Sylvester.tf

open System

open TensorFlow

open Sylvester

type Graph(nameScope:string) = 
    inherit Sylvester.Graphs.Graph()
    
    let tfGraph = c_api.TF_NewGraph() |?? lazy failwith "Could not create new TF_Graph."
    
    do tfGraph.SetNameScope(nameScope)

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


  
 