namespace Sylvester

open Sylvester.Collections

/// Set of elements closed under some binary operation.
type Groupoid<'U when 'U: equality>(set:Set<'U>, op:'U->'U->'U) =
    inherit Struct<'U, Card.one>(set, arrayOf1 (Binary(op)))
    member val Set = set
    member x.Apply l r = op l r
    member x.ApplyStrict l r = 
        if set.Contains l && set.Contains r then
            op l r
        else failwithf "The set of this groupoid does not contain %A or %A" l r
    