namespace Sylvester

open System

open FSharp.Quotations

open Sylvester.Arithmetic
open Sylvester.Collections

type MapExpr<'t, 'u when 't: equality and 'u : equality> = Expr<'t->'u>

/// 0-ary operation between elements of type 't.
type NullaryOp<'t when 't : equality> = 't

/// Unary operation between elements of type t.
type UnaryOp<'t when 't: equality> = 't->'t

// Binary operation between elements of type t.
type BinaryOp<'t when 't: equality> = 't->'t->'t

type BinaryOp<'t, 'd when 't: equality and 'd: equality> = 't->'d->'d

// Order operation on elements of type t. true indicates less-than-or equal, false indicates greater than.
type Order<'t when 't: equality> = Expr<'t->'t->bool>

/// Union of operations between elements of type t.
type Op<'t when 't: equality> = 
| Nullary of NullaryOp<'t>
| Unary of UnaryOp<'t>
| Binary of BinaryOp<'t>
| Order of Order<'t>
with 
    (* tbd *)
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
    static member IsIdempotent (op:BinaryOp<'t>) = true
    static member FailIfNotIdempotent (op:BinaryOp<'t>) = if not (Op<'t>.IsIdempotent op) then failwith "This operation is not idempotent."
   
/// Collection of n operations between elements of type t.
type Ops<'n, 't when 'n :> Number and 't: equality> = Array<'n, Op<'t>>

[<AutoOpen>]
module Op =
    let is_commutative (op:BinaryOp<'t>) = Op<'t>.IsCommutative op
    let fail_if_not_commutative (op:BinaryOp<'t>) = Op<'t>.FailIfNotCommutative op
    let is_left_associative (op:BinaryOp<'t>) = Op<'t>.IsLeftAssociative (op.ToString())
    let fail_if_not_left_associative (op:BinaryOp<'t>) = Op<'t>.FailIfNotLeftAssociative op
    let distributes_over (op1:BinaryOp<'t>) (op2:BinaryOp<'t>) = Op<'t>.DistributesOver (op1.ToString()) (op2.ToString())
    let fail_if_not_distributive_over (op1:BinaryOp<'t>) (op2:BinaryOp<'t>) = Op<'t>.FailIfNotDistributiveOver op1 op2
    let is_idempotent (op:BinaryOp<'t>) = Op<'t>.IsIdempotent op
    let fail_if_not_idempotent(op:BinaryOp<'t>) = Op<'t>.FailIfNotIdempotent op