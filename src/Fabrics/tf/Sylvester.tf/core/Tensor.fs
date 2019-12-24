namespace Sylvester.tf

open TensorFlow

open Sylvester
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Tensors

type UnknownTensor(tfTensor:TF_Tensor) = 
    inherit Api()
    
    let ttype = tf_tensor.TF_TensorType(tfTensor) 
    let rank = tf_tensor.TF_NumDims(tfTensor)
    let dims = Array.create 4 3
    do base.Initialized <- true

    interface IUnknownShape with  
        member val KnownRank:Option<int> = None with get, set
        member val KnownDims:Option<int[]> = None with get, set
    
type PartialTensor<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1 when 'd10 :> Base10Digit and 'd9 :> Base10Digit 
and 'd8 :> Base10Digit and 'd7 :> Base10Digit and 'd6 :> Base10Digit
and 'd5 :> Base10Digit and 'd4 :> Base10Digit and 'd3 :> Base10Digit and 'd2 :> Base10Digit 
and 'd1 :> Base10Digit>(tfTensor:TF_Tensor) = 
    inherit UnknownTensor(tfTensor)
    
    interface IPartialShape<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1> with
        member x.Rank = N10<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>()
        

