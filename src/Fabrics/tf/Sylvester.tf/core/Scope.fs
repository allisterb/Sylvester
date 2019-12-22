namespace Sylvester.tf

open TensorFlow
open Sylvester

type Scope(name:string) = 
    inherit Sylvester.Graph()
    
    member __.Name = name
    member __.Status = tf_status.TF_NewStatus() |?? failwith "Could not create TF_Status object."

        

    
