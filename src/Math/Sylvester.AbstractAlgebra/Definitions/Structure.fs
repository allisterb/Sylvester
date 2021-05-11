namespace Sylvester

open Arithmetic

/// A mathematical structure consisting of a set together with a collection of n closed operations on elements of type t.
type IStruct<'t, 'n when 't: equality and 'n :> Number> = 
    inherit ISet<'t>
    abstract Ops: Ops<'n, 't>
     
/// Base implementation of a mathematical structure consisting of a set together with a collection of n closed operations on elements of type t.
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