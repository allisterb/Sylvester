namespace Sylvester.AbstractAlgebra

open System

open Sylvester.Arithmetic
open Sylvester.Collections

/// Map or function between elements of universe U.
type Map<'U when 'U: equality> = 'U -> 'U 

/// Collection of maps.
type Maps<'n, 'U when 'n :> Number and 'U: equality> = Array<'n, Map<'U>>

/// A set together with a collection of maps of elements in some universe U.
type IStruct<'U, 'n when 'U: equality and 'n :> Number> = 
    abstract member Object:Set<'U>
    abstract member Maps:Maps<'n, 'U>

/// Base implementation of a structured set that is inherited by other mathematical structures.
type Struct<'U, 'n when 'U: equality and 'n :> Number>
    (object: Set<'U>, maps: Maps<'n, 'U>) = 
    interface IStruct<'U, 'n> with 
        member val Object = object
        member val Maps = maps
