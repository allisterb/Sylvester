namespace Sylvester

open System
open System.Collections

open Sylvester.Arithmetic
open Sylvester.Collections

/// Map or function between elements of type t.
type Map<'t when 't: equality> = 't -> 't  

/// Map or function between elements of types t->u.
type Map<'t, 'u when 't: equality and 'u : equality> = 't -> 'u 

/// Map or function between elements of types t->u->v.
type Map<'t, 'u, 'v when 't: equality and 'u : equality and 'v: equality> = 't -> 'u -> 'v 

/// 0-ary operation between elements of type 't.
type NullaryOp<'t when 't : equality> = 't

/// Unary operation between elements of type t.
type UnaryOp<'t when 't: equality> = Map<'t, 't>

// Binary operation between elements of type t.
type BinaryOp<'t when 't: equality> = Map<'t, 't, 't>

// Order operation on elements of type t.
type Order<'t when 't: equality> = Map<'t, 't, bool>

/// Union of operations between elements of type t.
type Op<'t when 't: equality> = 
| Nullary of NullaryOp<'t>
| Unary of UnaryOp<'t>
| Binary of BinaryOp<'t>
| Order of Order<'t>
with 
    member x.DestructureNullary = match x with | Nullary op -> op | _ -> failwith "This operation is not a nullary op."
    member x.DestructureUnary = match x with | Unary op -> op | _ -> failwith "This operation is not a unary op."
    member x.DestructureBinary = match x with | Binary op -> op | _ -> failwith "This operation is not a binary op." 
    static member LeftAssociative = ["+"; "-"; "*"; "/"] 
    static member IsLeftAssociative op = true
    static member FailIfNotLeftAssociative op = if not (Op<'t>.IsLeftAssociative op) then failwith "This operation is not left-associative."
    static member IsCommutative op1 = true
    static member FailIfNotCommutative op = if not (Op<'t>.IsCommutative op) then failwith "This operation is not left-associative."
    static member DistributesOver op1 op2 = true
    static member FailIfNotDistributiveOver op1 op2 = if not (Op<'t>.DistributesOver op1 op2) then failwith "This operation is not distributive."
   
/// Collection of n operations between elements of type t.
type Ops<'n, 't when 'n :> Number and 't: equality> = Array<'n, Op<'t>>

[<AutoOpen>]
module Op =
    let inline isCommutative (op:BinaryOp<'t>) = Op<'t>.IsCommutative op
    let inline failIfNotCommutative (op:BinaryOp<'t>) = Op<'t>.FailIfNotCommutative op
    let iisLeftAssociative (op:BinaryOp<'t>) = Op<'t>.IsLeftAssociative (op.ToString())
    let failIfNotLeftAssociative (op:BinaryOp<'t>) = Op<'t>.FailIfNotLeftAssociative op
    let inline distributesOver (op1:BinaryOp<'t>) (op2:BinaryOp<'t>) = Op<'t>.DistributesOver (op1.ToString()) (op2.ToString())
    let inline failIfNotDistributiveOver (op1:BinaryOp<'t>) (op2:BinaryOp<'t>) = Op<'t>.FailIfNotDistributiveOver op1 op2
