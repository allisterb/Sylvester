namespace Sylvester

open Sylvester.Collections

type IGroupoid<'U when 'U: equality> = 
    inherit IStruct<'U, card.one>
    abstract member Op: ('U -> 'U -> 'U)

/// Set of elements closed under some binary operation.
type Groupoid<'U when 'U: equality>(set:Set<'U>, op:'U->'U->'U) =
    inherit Struct<'U, card.one>(set, arrayOf1 (Binary(op)))
    member val Op = op
    interface IGroupoid<'U> with
        member val Op = op
    