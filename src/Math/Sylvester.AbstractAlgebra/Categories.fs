namespace Sylvester.AbstractAlgebra

open System

open Sylvester.Arithmetic
open Sylvester.Collections

/// Map or function between numbers of the same type.
type Map<'U when 'U : struct  and 'U: equality and 'U :> IFormattable> = 'U -> 'U 
   
/// Morphism between 2 sets of the same type.
type Morph<'U when 'U : struct  and 'U: equality and 'U :> IFormattable> = 
/// Single morphism defined by a map or function from members of one set to another.
| Morph of Set<'U> * Set<'U> * Map<'U>
/// Hom-set of all morphisms between 2 sets
| Hom of Set<'U> * Set<'U>
with       
    member x.Domain = 
        match x with
        |Morph(d, _, _) -> d
        |Hom(d,_) -> d
  
    member x.CoDomain = 
        match x with
        |Morph(_, c, _) -> c
        |Hom(_,c) -> c

    member x.Map = 
        match x with
        |Morph(_, _, m) -> m
        |Hom(_,_) -> failwith "The hom-set comprises all of the maps between 2 sets."
    
    static member (*) (l:Morph<'U>, r:Morph<'U>) =
        match l, r with
        | Morph(a, _, m), Morph(_, c, n) -> Morph(a, c, m >> n) 
        | _ -> failwith "Only individual morphisms are associative."
        
    /// Identity morphism
    static member Id(s) = Morph(s, s, id)

type Morphisms<'n, 'U when 'n :> Number and 'U : struct  and 'U: equality and 'U :> IFormattable> = Array<'n, Morph<'U>>

/// A collection of sets and collection of morphisms in some universe U.
type ICategory<'U, 'obn, 'mn when 'U : struct  and 'U: equality and 'U :> IFormattable and 'obn :> Number and 'mn :> Number> = 
    abstract member Objects:Sets<'obn, 'U>
    abstract member Morphisms:Morphisms<'mn, 'U>

/// Base implementation of a category of sets and morphisms in some universe U used by structures.
type Category<'U, 'obn, 'mn when 'U : struct  and 'U: equality and 'U :> IFormattable and 'obn :> Number and 'mn :> Number>
    (objects: Sets<'obn, 'U>, morphisms: Morphisms<'mn, 'U>) = 
    interface ICategory<'U, 'obn, 'mn> with 
        member val Objects = objects
        member val Morphisms = morphisms
