namespace Sylvester

open Sylvester.Arithmetic.N10

/// Set of elements closed under some left-associative operation.
type Semigroup<'U when 'U: equality>(set:Set<'U>, op:'U->'U->'U) =
    inherit Struct<'U>(set, Binary(op))
    do if (Op<'U>.IsLeftAssociative op) |> not then failwithf "The operator %A is not left-associative." op
    
/// Category of monoids with a structure-preserving morphism.
type Semigroups<'U when 'U : equality> = Category<'U, Semigroup<'U>, one>