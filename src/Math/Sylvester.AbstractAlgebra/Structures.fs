namespace Sylvester.AbstractAlgebra

open System

open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Collections

/// Collection of things.
type Collection<'n, 'c when 'n :> Number> = Array<'n, 'c>

/// Map or function between things.
type Map<'t> = 't -> 't 
   
/// Binary opertation between numbers.
type BinaryOp<'t when 't : struct  and 't: equality and 't :> IFormattable> = 't -> 't -> 't

/// Morphism between 2 sets of numbers.
type Morph<'t when 't : struct  and 't: equality and 't :> IFormattable>
    (domain:Set< 't>, codomain:Set< 't>, map:Map<'t>) = 
    member val Domain = domain
    member val CoDomain = codomain
    member val Map = map
    static member inline Id(domain, codomain) = Morph(domain, codomain, id)
    static member inline Zero = Morph<'t>.Id(Set.Empty, Set.Empty)
    
/// Structure of sets of numbers and binary operations.
type Struct<'s, 'ops, 't when 's :> Number and 'ops :> Number and 't : struct  and 't: equality and 't :> IFormattable>
    (sets: Collection<'s, Set<'t>>, ops: Collection<'ops, BinaryOp<'t>>) = 
    member val Sets = sets
    member val Ops = ops

/// Category of objects and morphisms.
type Category<'s, 'm, 't when 's :> Number and 'm :> Number and 't : struct  and 't: equality and 't :> IFormattable>
    (objects: Collection<'s, Set<'t>>, morphisms: Collection<'m, Morph<'t>>) = 
    member val Objects = objects
    member val Morphisms = morphisms
    static member inline Zero = Category(Collection<zero,Set<int>>(), Collection<zero, Morph<_>>())
    