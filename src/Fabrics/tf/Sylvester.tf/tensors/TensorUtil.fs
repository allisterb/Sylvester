namespace Sylvester.tf

open System
open System.Linq

open TensorFlow

open Sylvester
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Collections
open Sylvester.Tensors
open Sylvester.Graphs
open Sylvester.tf

module TensorUtil =
    let typeToDataType(t:Type) =
        match t.Name with
        | "System.Boolean" -> TF_DataType.TF_BOOL, 1
        | "System.SByte" -> TF_DataType.TF_INT8, 1
        | "System.Byte" -> TF_DataType.TF_UINT8, 1
        | "System.Int16" -> TF_DataType.TF_INT16, 2
        | "System.UInt16" -> TF_DataType.TF_UINT16, 2
        | "System.Int32" -> TF_DataType.TF_INT32, 4
        | "System.UInt32" -> TF_DataType.TF_UINT32, 4
        | "System.Int64" -> TF_DataType.TF_INT64, 8
        | "System.UInt64" -> TF_DataType.TF_UINT64, 8
        | "System.Single" -> TF_DataType.TF_FLOAT, 8
        | "System.Double" -> TF_DataType.TF_DOUBLE, 8
        | _ -> failwithf "The type %s is not a numeric type." t.Name

    
    let arrayTotalLength (arr:Array) : int =
        let mutable length = 1
        for i in 1..arr.Rank do
            length <- length * arr.GetLength(i - 1)
        length

    let arrayIsJagged (arr:Array) = 
        if arr.Length = 0 then
            arr.Rank = 1
        else
            arr.Rank = 1 && arr.GetValue(0) :? Array
        
    let typeIsJagged (t:Type) = t.IsArray && t.GetElementType().IsArray

    let arrayInnermostType (array:Array) =
        let rec innermost (t:Type) = if t.IsArray then innermost (t.GetElementType()) else t
        innermost <| array.GetType()
        
