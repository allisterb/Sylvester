namespace Sylvester.AbstractAlgebra

open System

open Sylvester.Arithmetic.N10
open Sylvester.Collections

/// Set of elements closed under some operation.
type Monoid<'U when 'U: equality>(set:Set<'U>, op:Map<'U>) =
    inherit Category<'U, one, one>(arrayOf1 set, arrayOf1 (Morph(set, set, op)))