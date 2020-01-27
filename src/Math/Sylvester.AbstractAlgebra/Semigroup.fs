namespace Sylvester

open Sylvester.Collections

/// Set of elements closed under some left-associative operation.
type Semigroup<'U when 'U: equality>(set:Set<'U>, op:'U->'U->'U) =
    inherit Struct<'U, Card.one>(set, Binary(op) |> arrayOf1)
    do if (Op<'U>.IsLeftAssociative op) |> not then failwithf "The operator %A is not left-associative." op
    
/// Category of monoids with a structure-preserving morphism.
type Semigroups<'U when 'U : equality> = Category<'U, Semigroup<'U>, Card.one>