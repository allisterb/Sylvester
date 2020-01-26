namespace Sylvester.Collections

open System
open System.Collections.Generic

open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10


/// Collection of n heterogeneous or homogenous elements with type-level cardinality.
type ICollection<'n when 'n :> Number> = abstract member Card:'n

type Empty = Empty with interface ICollection<zero> with member x.Card = zero
