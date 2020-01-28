namespace Sylvester

open System
open System.Collections

open Sylvester.Arithmetic
open Sylvester.Collections

type IMap<'n when 'n :> Number> = interface end

/// Map or function between elements of universe U.
type Map<'U when 'U: equality> = 'U -> 'U  

/// Unarp operation between elements of universe U.
type UnaryOp<'U when 'U: equality> = 'U -> 'U  

/// Binary operation between pairs of elements of universe U.
type BinaryOp<'U when 'U: equality> = 'U -> 'U -> 'U 

/// Union of operations between elements of a universe U.
type Op<'U> = 
|Unary of ('U -> 'U) 
|Binary of ('U -> 'U -> 'U)
with
    static member LeftAssociativeSymbols = ["+"; "-"; "*"; "/"] 
    static member inline IsLeftAssociative op = Op<'U>.LeftAssociativeSymbols |> Seq.contains (op.ToString())
    static member inline FailIfNotLeftAssociative op = if not (Op<'U>.IsLeftAssociative op) then failwith "This operation is not left-associative."
    static member inline IsCommutative op1 = true
    static member inline FailIfNotCommutative op = if not (Op<'U>.IsCommutative op) then failwith "This operation is not left-associative."
    static member inline DistributesOver op1 op2 = true
    static member inline FailIfNotDistributiveOver op1 op2 = if not (Op<'U>.DistributesOver op1 op2) then failwith "This operation is not distributive."

    member x.Symbol = match x with | Unary op -> op.ToString() | Binary op -> op.ToString()

    member inline x.DestructureUnary = match x with | Unary op -> op | _ -> failwith "This operation is not a unary op."
    member inline x.DestructureBinary = match x with | Binary op -> op | _ -> failwith "This operation is not a binary op." 
    
/// Collection of n operations.
type Ops<'n, 'U when 'n :> Number and 'U: equality> = Array<'n, Op<'U>>

[<AutoOpen>]
module Op =
    let inline isLeftAssociative (op:BinaryOp<'U>) = Op<'U>.IsLeftAssociative op
    let inline failIfNotLeftAssociative (op:BinaryOp<'U>) = Op<'U>.FailIfNotLeftAssociative op
    let inline isCommutative (op:BinaryOp<'U>) = Op<'U>.IsCommutative op
    let inline failIfNotCommutative (op:BinaryOp<'U>) = Op<'U>.FailIfNotCommutative op
    let inline distributesOver (op1:BinaryOp<'U>) (op2:BinaryOp<'U>) = Op<'U>.DistributesOver op1 op2
    let inline failIfNotDistributiveOver (op1:BinaryOp<'U>) (op2:BinaryOp<'U>) = Op<'U>.FailIfNotDistributiveOver op1 op2
