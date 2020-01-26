namespace Sylvester.AbstractAlgebra

open System

open Sylvester.Arithmetic
open Sylvester.Collections

/// Map or function between elements of universe U.
type Map<'U when 'U: equality> = 'U -> 'U 

/// Map or function between elements of universe U and universe V.
type Map<'U, 'V when 'U: equality and 'V: equality> = 'U -> 'V 

/// Operations between elements of a universe U.
type Op<'U> = 
|Unary of ('U -> 'U) 
|Binary of ('U -> 'U -> 'U)
with
    member x.Symbol = match x with | Unary op -> op.ToString() | Binary op -> op.ToString()
    static member LeftAssociativeSymbols = ["+"; "-"] 
    static member inline IsLeftAssociative op = Op<'U>.LeftAssociativeSymbols |> Seq.contains (op.ToString())
    
/// Collection of n operations.
type Ops<'n, 'U when 'n :> Number and 'U: equality> = Array<'n, Op<'U>>

/// A set together with a collection of n operations of elements in some universe U.
type IStruct<'U, 'n when 'U: equality and 'n :> Number> = 
    abstract member Set:Set<'U>
    abstract member Ops:Ops<'n, 'U>

/// Base implementation of a mathematical structure consisting of a set together with a collection of n operations of elements in some universe U.
/// This type is inherited by other mathematical structure types.
type Struct<'U, 'n when 'U: equality and 'n :> Number>(set: Set<'U>, ops: Ops<'n, 'U>) = 
    interface IStruct<'U, 'n> with 
        member val Set = set
        member val Ops = ops

    member x.Set = set
    member x.Ops = ops