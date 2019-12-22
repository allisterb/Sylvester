namespace Sylvester.tf

open TensorFlow
open Sylvester

type Graph() = 
    inherit Sylvester.Graph()
    
    let tfGraph = c_api.TF_NewGraph() |?? failwith "Could not create new TF_Graph."
    
    let scope = Scope("")
    
    do tfGraph.SetNameScope(scope.Name)

    member __.Scope = scope

    member __.NewSubScope = __.Scope

    member __.UpdateStatus(status:TF_Status) = 
          __.Status <- {Code = tf_status.TF_GetCode(status); Message = tf_status.TF_Message(status)}
          do if __.Status.Code <> TF_Code.TF_OK then failwith "A scope operation did not return TF_OK."


    member val Status = {Code = TF_Code.TF_UNKNOWN; Message = ""} with get, set

and Scope(name:string) = 
    inherit Api()
    
    member __.Name = name

    member __.NewSubScope(subName:string) = if __.Name = "" then subName else __.Name + "/" + subName
    
     
and Status = {Code: TF_Code; Message: string}     


        

    
