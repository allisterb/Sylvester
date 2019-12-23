namespace Sylvester.tf

open TensorFlow

open Sylvester

type Operation(g:Graph, tfOperation: TF_Operation) = 
    inherit Sylvester.Graphs.Operation()
    
    let name = c_api.TF_OperationName(tfOperation) |?? lazy failwith "Could not get name of operation." 
    
    let opType = c_api.TF_OperationOpType(tfOperation) |?? lazy failwith "Could not get type of operation."

    let numInputs = c_api.TF_OperationNumInputs(tfOperation)

    let numOutputs = c_api.TF_OperationNumOutputs(tfOperation)
    
    do base.Initialized <- name <> null && opType <> null
    
    member x.Name = name

    member x.Type = OpType.FromString(opType)

    member x.NumInputs = numInputs

    member x.NumOutputs = numOutputs


and OpType =
    | Int
    | Float
    | Bool
    | DataType
    | Shape
    | Tensor
    | String
    | Function
    with 
    override x.ToString() =
        match x with
        | OpType.Int -> "int"
        | OpType.Float -> "float"
        | OpType.Bool -> "bool"
        | OpType.DataType -> "type"
        | OpType.Shape -> "shape"
        | OpType.Tensor -> "tensor"
        | OpType.String -> "string"
        | OpType.Function -> "func"

    static member FromString(s:string) = 
        match s with
        | "int" -> OpType.Int
        | "float" -> OpType.Float
        | "bool" -> OpType.Bool
        | "type" -> OpType.DataType
        | "shape" -> OpType.Shape
        | "tensor" -> OpType.Tensor
        | "string" -> OpType.String
        | "func" -> OpType.Function
        | _ -> failwithf "Unknown TensorFlow type %s" s

    static member op_Explicit(o:OpType) : string = o.ToString()


        

    
