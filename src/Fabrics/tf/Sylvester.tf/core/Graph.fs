namespace Sylvester.tf

open TensorFlow
open Sylvester

type Graph(scope:Scope) = 
    inherit Sylvester.Graph()
    
    let tfGraph = c_api.TF_NewGraph() |?? failwith "Could not create new TF_Graph."
    
    do tfGraph.SetScope(scope.Name)

    member __.Scope = tfGraph.NameScope

        

    
