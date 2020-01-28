namespace Sylvester

open System

open Sylvester.Arithmetic
open Sylvester.Collections

/// Map or function between elements of universe U.
type Map<'U when 'U: equality> = 'U -> 'U 

/// Operations between elements of a universe U.
type Op<'U> = 
|Unary of ('U -> 'U) 
|Binary of ('U -> 'U -> 'U)
with
    member x.Symbol = match x with | Unary op -> op.ToString() | Binary op -> op.ToString()
    static member LeftAssociativeSymbols = ["+"; "-"; "*"; "/"] 
    static member inline IsLeftAssociative op = Op<'U>.LeftAssociativeSymbols |> Seq.contains (op.ToString())
    static member inline IsCommutative op1 = true
    static member inline DistributesOver op1 op2 = true

/// Collection of n operations.
type Ops<'n, 'U when 'n :> Number and 'U: equality> = Array<'n, Op<'U>>

/// A set together with a collection of n operations of elements in some universe U.
type IStruct<'U, 'n when 'U: equality and 'n :> Number> = 
    abstract member Set:Set<'U>
    abstract member Ops:Ops<'n, 'U>

/// Base implementation of a mathematical structure consisting of a set together with a collection of n operations on elements in some universe U.
/// This type is inherited by all other mathematical structure types.
type Struct<'U, 'n when 'U: equality and 'n :> Number>(set: Set<'U>, ops: Ops<'n, 'U>) =  
    member val Set = set
    member val Ops = ops
    interface IStruct<'U, 'n> with
        member val Set = set
        member val Ops = ops
    
/// Properties of operations.
[<AutoOpen>]
module Op =
    let isLeftAssociative (op: 'U->'U->'U) = Op<'U>.IsLeftAssociative op
    let isCommutative (op: 'U->'U->'U) = Op<'U>.IsCommutative op
    let distributesOver (op1: 'U->'U->'U) (op2 : 'U->'U->'U)= Op<'U>.DistributesOver op1 op2

/// The cardinality of a structure's collections.
[<RequireQualifiedAccess>]
module card = 
    /// Cardinality 0.
    type zero = N10.zero

    /// Cardinality 1.
    type one = N10.one

    /// Cardinality 2.
    type two = N10.two

    /// Cardinality 3.
    type three = N10.three

    /// Cardinality 4.
    type four = N10.four

    /// Cardinality 5.
    type five = N10.five

    /// Cardinality 6.
    type six = N10.six

    /// Cardinality 7.
    type seven = N10.seven

    /// Cardinality 8.
    type eight = N10.eight

    /// Cardinality 9.
    type nine = N10.nine

    /// Cardinality 10.
    type ten = N10.ten

    let zero = new zero()

    let one = new one()

    let two = new two()

    let three = new three()

    let four = new four()

    let five = new five()

    let six = new six()

    let seven = new seven()

    let eight = new eight()

    let nine = new nine()

    let ten = new ten()

