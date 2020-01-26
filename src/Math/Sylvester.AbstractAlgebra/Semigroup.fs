namespace Sylvester.AbstractAlgebra

open System

open Sylvester.Arithmetic.N10
open Sylvester.Collections

/// Set of elements closed under some left-associative operation.
type Semigroup<'U when 'U: equality>(set:Set<'U>, op:'U->'U->'U) =
    inherit Struct<'U, one>(set, arrayOf1 (Binary(op)))
    do if (Op<'U>.IsLeftAssociative op) |> not then failwithf "The operator %A is not left-associative." op
    
/// Category of monoids with a structure-preserving morphism.
type Semigroups<'U when 'U : equality> = Category<'U, Semigroup<'U>, one>