namespace Sylvester.AbstractAlgebra

open System

open Sylvester.Arithmetic
open Sylvester.Collections

/// Structure of elements of R that have arithmetical properties.
type Struct<'t when 't : struct  and 't: comparison and 't: equality and 't :> IFormattable> internal (x: R<'t>) = 
    member x.Set = x

/// Binary operation between elements of R that have arithmetical properties.
type BinaryOp<'t when 't : struct  and 't: comparison and 't: equality and 't :> IFormattable> = 't -> 't -> 't
    
/// Morphism between structures.
type Morph<'t when 't : struct  and 't: comparison and 't: equality and 't :> IFormattable>
    internal (domain:Struct< 't>, codomain:Struct< 't>, op:BinaryOp<'t>) = 
    member val Domain = domain
    member val CoDomain = codomain
    member val Op = op
    
type Category<'s, 'm, 't when 's :> Number and 'm :> Number and 't : struct  and 't: comparison and 't: equality and 't :> IFormattable>
    (structures: Array<'s, Struct<'t>>, morphisms: Array<'s, Morph<'t>>) = 
    
    member val Structures = structures
    member val Morphisms = morphisms

[<AutoOpen>]
module Categories = 
    /// Construct a structure with the correct type constraints
    let inline Struct<'t when 't : struct  and 't: comparison and 't: equality and 't :> IFormattable
        and 't : (static member (+): 't -> 't -> 't)
        and 't : (static member (-): 't -> 't -> 't)
        and 't : (static member (*): 't -> 't -> 't)
        and 't : (static member (/): 't -> 't -> 't)
        and 't : (static member Zero: 't)
        and 't : (static member One: 't)> (x:R<'t>) = Struct(x) 

    let inline Morph<'t when 't : struct  and 't: comparison and 't: equality and 't :> IFormattable
        and 't : (static member (+): 't -> 't -> 't)
        and 't : (static member (-): 't -> 't -> 't)
        and 't : (static member (*): 't -> 't -> 't)
        and 't : (static member (/): 't -> 't -> 't)
        and 't : (static member Zero: 't)
        and 't : (static member One: 't)> (x:R<'t>, y:R<'t>, op: BinaryOp<'t>) = Morph(Struct(x), Struct(y), op) 
