namespace Sylvester.Collections

open Sylvester
open N10

type HCollection<'a> = Singleton of 'a 
with 
    member x.Elems = let (Singleton a) = x in a
    member x.Card = ``1``
    interface ICollection<``1``> with member x.Card = ``1``

type HCollection<'a, 'b> = Pair of 'a * 'b
with 
    member x.Elems = let (Pair(a, b)) = x in (a, b)
    member x.Card = ``2``
    interface ICollection<``2``> with member x.Card = ``2``

type HCollection<'a, 'b, 'c> = Triple of 'a * 'b * 'c
with 
    member x.Elems = let (Triple(a, b, c)) = x in (a, b, c)
    member x.Card = ``3``
    interface ICollection<``3``> with member x.Card = ``3``

type HCollection<'a, 'b, 'c, 'd> = Tuple4 of 'a * 'b * 'c * 'd
with 
    member x.Elems = let (Tuple4(a, b, c, d)) = x in (a, b, c, d)
    member x.Card = ``4``
    interface ICollection<``4``> with member x.Card = ``4``

type HCollection<'a, 'b, 'c, 'd, 'e> = Tuple5 of 'a * 'b * 'c * 'd * 'e
with 
    member x.Elems = let (Tuple5(a, b, c, d, e)) = x in (a, b, c, d, e)
    member x.Card = ``5``
    interface ICollection<``5``> with member x.Card = ``5``