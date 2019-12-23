namespace Sylvester.tf

open TensorFlow

open Sylvester
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Tensors

type PartialTensor(tfTensor:TF_Tensor) = 
    inherit Api()
    
    let tfTensorType = tf_tensor.TF_TensorType(tfTensor) 
    do base.Initialized <- true

    interface IPartialTensor
    
type RuntimeTensor(tfTensor:TF_Tensor) =
    inherit PartialTensor(tfTensor)
    let rank = tf_tensor.TF_NumDims(tfTensor)
    let dims = Array.create(1) 

    interface IRuntimeTensor with
        member x.Rank = rank
        member x.Dims = dims

type Tensor<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1 when 'd10 :> Base10Digit and 'd9 :> Base10Digit 
and 'd8 :> Base10Digit and 'd7 :> Base10Digit and 'd6 :> Base10Digit
and 'd5 :> Base10Digit and 'd4 :> Base10Digit and 'd3 :> Base10Digit and 'd2 :> Base10Digit 
and 'd1 :> Base10Digit>(tfTensor:TF_Tensor) = 
    inherit RuntimeTensor(tfTensor)
    
    interface ITensor<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1> with
        member x.Rank = N10<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>()
        

