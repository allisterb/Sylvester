namespace Sylvester

open Sylvester.Arithmetic

/// A mathematical structure consisting of a set together with a collection of n operations on elements of type 't.
type IStruct<'t, 'n when 't: equality and 'n :> Number> = 
    inherit ISet<'t>
    abstract Ops: Ops<'n, 't>
   
/// Base implementation of a mathematical structure consisting of a set together with a collection of n operations on elements of type t.
/// This type is inherited by all other mathematical structure types.
type Struct<'t, 'n when 't: equality and 'n :> Number>(set: ISet<'t>, ops: Ops<'n, 't>) =  
    member val Set = set.Set
    member val Ops = ops    
    interface IStruct<'t, 'n> with
        member val Set = set.Set
        member x.Equals y = x.Set.Equals y
        member val Ops = ops

/// A structure that has an identity element.
type IIdentity<'t when 't : equality> = 
    abstract member Identity:'t

/// A structure that has an identity element.
type IInverse<'t when 't : equality> = 
    abstract member Inverse:UnaryOp<'t>

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

    type aleph0 = N10.ten

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

    