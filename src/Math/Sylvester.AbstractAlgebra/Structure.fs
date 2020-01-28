namespace Sylvester

open System
open System.Collections

open Sylvester.Arithmetic
open Sylvester.Collections

/// A set together with a collection of n operations of elements in some universe U.

type IStruct<'U, 'n when 'U: equality and 'n :> Number> = 
    inherit ISet<'U>
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
        member x.GetEnumerator() : Generic.IEnumerator<'U> = (x.Set :> Generic.IEnumerable<'U>).GetEnumerator()
        member x.GetEnumerator() : IEnumerator = (x.Set :> IEnumerable).GetEnumerator()
        member x.Sub f = (x.Set :> ISet<'U>).Sub f
        member x.Contains e = (x.Set :> ISet<'U>).Contains e
    
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

