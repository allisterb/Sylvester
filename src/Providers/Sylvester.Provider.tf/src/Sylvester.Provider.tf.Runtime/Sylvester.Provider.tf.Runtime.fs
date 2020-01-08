namespace Sylvester.tf

open System

open TensorFlow

// Put the TypeProviderAssemblyAttribute in the runtime DLL, pointing to the design-time DLL
[<assembly:CompilerServices.TypeProviderAssembly("Sylvester.Provider.tf.DesignTime.dll")>]
do ()

[<AutoOpen>]
module DataTypes =
    [<Literal>] 
    let BOOL = 10  // TF_DataType.TF_BOOL
    
    [<Literal>] 
    let INT8 =  6  // TF_DataType.TF_INT8
    
    [<Literal>] 
    let UINT8 = 4  // TF_DataType.TF_UINT8
    
    [<Literal>] 
    let INT16 = 5  // TF_DataType.TF_INT16

    [<Literal>] 
    let UINT16 = 17 // TF_DataType.TF_UINT16

    [<Literal>] 
    let INT32 = 3   // TF_DataType.TF_INT32

    [<Literal>] 
    let UINT32 = 22 // TF_DataType.TF_UINT32

    [<Literal>] 
    let INT64 = 9   // TF_DataType.TF_INT64

    [<Literal>] 
    let UINT64 = 23 // TF_DataType.TF_UINT64

    [<Literal>] 
    let FLOAT = 1   // TF_DataType.TF_FLOAT

    [<Literal>] 
    let DOUBLE = 2  // TF_DataType.TF_DOUBLE

    let dataType (t:int) =
        match t with
        | BOOL -> typeof<bool>
        | INT8 -> typeof<sbyte>
        | UINT8 -> typeof<byte>
        | INT16 -> typeof<int16>
        | UINT16 -> typeof<uint16>
        | INT32 -> typeof<int32>
        | UINT32 -> typeof<uint32>
        | INT64 -> typeof<int64>
        | UINT64 -> typeof<uint64>
        | FLOAT -> typeof<float32>
        | DOUBLE -> typeof<double>
        | _ -> failwithf "Cannot convert data type code %i to TensorFlow type." t
    
