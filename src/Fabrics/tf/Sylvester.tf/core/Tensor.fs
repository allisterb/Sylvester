namespace Sylvester.tf

open TensorFlow

open Sylvester

type PartialTensor(tfTensor:TF_Tensor) = 
    inherit Api()
    
    let tensorType = tf_tensor.TF_TensorType(tfTensor) 
    
    do base.Initialized <- true
    
type RuntimeTensor(tfTensor:TF_Tensor) =
    inherit PartialTensor(tfTensor)

    let dims = tf_tensor.TF_NumDims(tfTensor)

    let c = seq{for i 0..}
        

    
