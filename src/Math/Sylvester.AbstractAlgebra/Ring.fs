namespace Sylvester

open Sylvester.Arithmetic.N10
open Sylvester.Collections

type IRing<'U when 'U: equality> = 
    inherit IStruct<'U, card.two> 
    abstract member Group1: Group<'U>
    abstract member Group2: Group<'U>

/// Set of elements closed under 2 left-associative commutative operations with identities and inverses where one operation distributes over another.
type Ring<'U when 'U: equality>(set:Set<'U>, group1: Group<'U>, group2: Group<'U>) =
    inherit Struct<'U, card.two>(set, arrayOf2 (group1.Ops.[zero]) (group2.Ops.[zero]))
    do if not(Op.distributesOver group2.Op group1.Op) then failwith ""

    member val Op1 = group1.Op
    member val Op2 = group2.Op
    interface IRing<'U> with
        member val Group1 = group1
        member val Group2 = group2

