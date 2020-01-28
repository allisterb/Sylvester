namespace Sylvester

open Sylvester.Collections
    
/// Set of elements closed under some left-associative operation.
type Semigroup<'U when 'U: equality>(set:Set<'U>, op:'U->'U->'U) =
    inherit Groupoid<'U>(set, op)
    do if op |> isLeftAssociative |> not then failwithf "The operator %A is not left-associative." op
    
/// Category of semigroups with a structure-preserving morphism.
type Semigroups<'U when 'U : equality> = Category<'U, Semigroup<'U>, card.one>