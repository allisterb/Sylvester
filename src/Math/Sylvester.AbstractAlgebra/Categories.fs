namespace Sylvester.AbstractAlgebra

open System

open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Collections

/// Map or function between objects.
type Map<'domain, 'codomain> = 'domain -> 'codomain 
   
/// Composition of maps.
type Comp<'a, 'b, 'c> = Map<'a, 'b> -> Map<'b, 'c> -> Map<'a, 'c>

/// Morphism between 2 objects.
type Morph<'domain, 'codomain>(domain:'domain, codomain:'codomain, map:Map<'domain, 'codomain>) = 
    member val Domain = domain
    member val CoDomain = codomain
    member val Map = map
    
    static member inline Id(x:'domain) = Morph(x, x, id)  
    static member inline Zero = Morph(Empty, Empty, id)

/// Binary operation between numbers.
type BinaryOp<'t when 't : struct  and 't: equality and 't :> IFormattable> = 't -> 't -> 't
    
/// A collection of objects and collection of morphisms in some universe.
type Category<'obn, 'mn, 'ob, 'm, 'U when 'obn :> Number and 'mn :> Number and 'ob :> ICollection<'obn> and 'm  :> ICollection<'mn>>
    (objects: 'ob, morphisms: 'm) = 
    member val Objects = objects
    member val Morphisms = morphisms
    


    