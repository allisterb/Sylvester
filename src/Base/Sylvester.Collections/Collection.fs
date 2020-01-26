namespace Sylvester.Collections

open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10

/// Collection of n heterogeneous elements with type-level cardinality.
type ICollection<'n when 'n :> Number> = abstract member Card:'n

type Empty = Empty with interface ICollection<zero> with member x.Card = zero

type Collection<'a> = Singleton of 'a 
with 
    member x.Elems = let (Singleton a) = x in a
    member x.Card = one
    interface ICollection<one> with member x.Card = one

type Collection<'a, 'b> = Pair of 'a * 'b
with 
    member x.Elems = let (Pair(a, b)) = x in (a, b)
    member x.Card = two
    interface ICollection<two> with member x.Card = two

type Collection<'a, 'b, 'c> = Triple of 'a * 'b * 'c
with 
    member x.Elems = let (Triple(a, b, c)) = x in (a, b, c)
    member x.Card = three
    interface ICollection<three> with member x.Card = three

type Collection<'a, 'b, 'c, 'd> = Tuple4 of 'a * 'b * 'c * 'd
with 
    member x.Elems = let (Tuple4(a, b, c, d)) = x in (a, b, c, d)
    member x.Card = four
    interface ICollection<four> with member x.Card = four

type Collection<'a, 'b, 'c, 'd, 'e> = Tuple5 of 'a * 'b * 'c * 'd * 'e
with 
    member x.Elems = let (Tuple5(a, b, c, d, e)) = x in (a, b, c, d, e)
    member x.Card = five
    interface ICollection<five> with member x.Card = five