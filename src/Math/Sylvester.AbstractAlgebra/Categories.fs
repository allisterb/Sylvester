namespace Sylvester.AbstractAlgebra

open System

open Sylvester.Arithmetic
open Sylvester.Collections

/// Structure over a set of numbers
type Struct<'t when 't : struct  and 't: comparison and 't: equality and 't :> IFormattable> (set: Set<'t>) = 
    member x.Set = set

/// Binary operation between numbers.
type BinaryOp<'t when 't : struct  and 't: comparison and 't: equality and 't :> IFormattable> = 't -> 't -> 't
    
/// Morphism between structures of numbers that have a defined binary operator.
type Morph<'t when 't : struct  and 't: comparison and 't: equality and 't :> IFormattable>
    (domain:Struct< 't>, codomain:Struct< 't>, op:BinaryOp<'t>) = 
    member val Domain = domain
    member val CoDomain = codomain
    member val Op = op

/// Category of structures and morphisms.
type Category<'s, 'm, 't when 's :> Number and 'm :> Number and 't : struct  and 't: comparison and 't: equality and 't :> IFormattable>
    (structures: Array<'s, Struct<'t>>, morphisms: Array<'s, Morph<'t>>) = 
    member val Structures = structures
    member val Morphisms = morphisms