namespace Sylvester

open Sylvester.Arithmetic.N10
open Sylvester.Collections
/// Set of elements closed under some operation with identity.
type Monoid<'U when 'U: equality>(set:Set<'U>, op:'U->'U->'U, id:'U) =
    inherit Groupoid<'U>(set, op)

/// Category of monoids with a structure-preserving morphism.
type Mon<'U when 'U : equality> = Category<'U, Monoid<'U>, one>