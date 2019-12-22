namespace Sylvester.tf

open TensorFlow
open Sylvester

type Graph(scope:Scope) = 
    inherit Sylvester.Graph()
    
    let tfGraph = c_api.TF_NewGraph() |?? lazy failwith "Could not create new TF_Graph."
    
    do tfGraph.SetNameScope(scope.Name)

    do base.Initialized <- tfGraph <> null

    new () = Graph(Scope.root())

    member __.Scope = scope

    member __.NameScope = scope.Name

    member __.SubScope = __.Scope.NewSubScope

    member __.UpdateStatus(status:TF_Status) = 
          __.Status <- {Code = tf_status.TF_GetCode(status); Message = tf_status.TF_Message(status)}
          do if __.Status.Code <> TF_Code.TF_OK then failwith "An operation in this scope did not return TF_OK."


    member val Status = {Code = TF_Code.TF_UNKNOWN; Message = ""} with get, set

and Scope(name:string) = 
    inherit Api()
    
    member __.Name = name

    member __.NewSubScope(subName:string) = if __.Name = "" then subName else __.Name + "/" + subName

    static member root() = Scope("")
    
     
and Status = {Code: TF_Code; Message: string}     


        

    
