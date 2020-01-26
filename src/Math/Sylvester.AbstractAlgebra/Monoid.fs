namespace Sylvester.AbstractAlgebra

open System

open Sylvester.Arithmetic.N10
open Sylvester.Collections

/// Set of elements closed under some operation.
type Monoid<'U when 'U: equality>(set:Set<'U>, op:Map<'U>, id:'U) =
    inherit Struct<'U, one>(set, arrayOf1 op)

/// Category of monoids with a structure-preserving morphism.
type Monoids<'U when 'U : equality>(m: Morph<'U, one>) = inherit Category<'U, Monoid<'U>, one>(m)