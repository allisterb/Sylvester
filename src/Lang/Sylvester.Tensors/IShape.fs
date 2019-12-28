namespace Sylvester.Tensors

open System

open Sylvester
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Collections

/// A tensor whose rank and dimensions may be unknown until runtime
type IUnknownShape =
    abstract member Rank:Option<int> with get,set
    abstract member Dims:Option<int64[]> with get,set

/// A tensor whose rank is known at compile time
type IPartialShape<'n when 'n :> Number> = 
    inherit IUnknownShape
    abstract member Rank:'n

/// A tensor whose rank and dimensions are known at compile time    
type IFullShape<'n when 'n :> Number> = 
    inherit IPartialShape<'n>
