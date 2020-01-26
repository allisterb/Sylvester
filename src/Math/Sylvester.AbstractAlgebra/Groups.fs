namespace Sylvester.AbstractAlgebra

open System

open Sylvester.Arithmetic.N10
open Sylvester.Collections

/// Set of numbers closed under some operation.
type Monoid<'U when 'U : struct  and 'U: comparison and 'U: equality and 'U :> IFormattable>(set:Set<'U>, op:Map<'U>) =
    inherit Category<'U, one, one, Sets<one, 'U>, Morphisms<one, 'U>>(arrayOf1 set, arrayOf1 (Morph(set, set, op)))
    
        