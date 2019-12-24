namespace Sylvester.tf

open System

open TensorFlow

open Sylvester

type Graph(scope:Scope) = 
    inherit Sylvester.Graphs.Graph()
    
    let tfGraph = c_api.TF_NewGraph() |?? lazy failwith "Could not create new TF_Graph."
    
    do tfGraph.SetNameScope(scope.Name)

    do base.Initialized <- tfGraph <> null

    member internal x._Graph = tfGraph

    member x.Scope = scope

    member x.NewSubScope = x.Scope.NewSubScope

    member x.NameScope
        with get() = tfGraph.NameScope
        and internal set(value) = tfGraph.SetNameScope(value)

    member x.MakeName = tfGraph.MakeName

    member val Status = {Code = TF_Code.TF_UNKNOWN; Message = ""} with get, set
    
    member x.UpdateStatus(status:TF_Status) = 
          x.Status <- {Code = tf_status.TF_GetCode(status); Message = tf_status.TF_Message(status)}
          do if x.Status.Code <> TF_Code.TF_OK then failwith "An operation in this scope did not return TF_OK."

    
    new () = Graph(Scope.NewRootScope())

    static member create() = Graph()

    static member create(scope:Scope) = Graph(scope)
    
and Scope(name:string) = 
    
    let prefix = if empty name then "" else name + "/"

    member x.Name = name

    member val Graph:Option<Graph> = None with get, set

    member x.NewSubScope(subName:string) = 
        if empty subName then failwith "New sub-scope name cannot be empty." else new Scope(prefix + subName)

    member x.WithOpName(opName:string, id:int) = prefix + opName + id.ToString()

    member x.WithOpName(opName:string, id:string) = prefix + opName + id

    new(g:Graph) as this = 
        new Scope(g.NameScope) then this.Graph <- Some g
                
    static member NewRootScope() = new Scope("")

and Status = {Code: TF_Code; Message: string}     


        

    
