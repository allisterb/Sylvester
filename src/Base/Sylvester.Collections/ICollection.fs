namespace Sylvester.Collections

open Sylvester
open Arithmetic

/// Collection of n heterogeneous or homogenous elements with type-level cardinality.
type ICollection<'n when 'n :> Number> = abstract member Card:'n

type Empty = Empty with interface ICollection<dim<0>> with member x.Card = new dim<0>()
