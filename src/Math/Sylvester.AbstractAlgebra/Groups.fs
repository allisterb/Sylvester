namespace Sylvester.AbstractAlgebra

open System

open Sylvester.Arithmetic.N10
open Sylvester.Collections

/// Group of numbers with arithmetical properties or objects isomorphic to such numbers.
type Monoid<'U when 'U : struct  and 'U: comparison and 'U: equality and 'U :> IFormattable>(set:Set<'U>) =
    inherit Category<one, zero, Collection<Set<'U>>, Empty, 'U>(Singleton(set), Empty)
    
        