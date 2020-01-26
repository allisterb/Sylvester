namespace Sylvester.AbstractAlgebra

open System

open Sylvester.Arithmetic.N10
open Sylvester.Collections

/// Set of elements closed under some operation.
type Monoid<'U when 'U: equality>(set:Set<'U>, op:Map<'U>) =
    inherit Struct<'U, one>(set, arrayOf1 op)