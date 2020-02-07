namespace Sylvester

open Sylvester.Arithmetic
open Sylvester.Collections

/// Morphism between 2 structures of type ut and ut which have un and vn operations respectively.
type Morph<'ut, 'vt, 'un, 'vn when 'ut : equality and 'vt : equality and 'un :> Number and 'vn :> Number> = 

/// Morphism defined by a map from elements of the set of one structure to another.
|Morph of Struct<'ut, 'un> * Struct<'vt, 'vn> * Map<'ut, 'vt> 

/// hom-set of all morphisms between 2 structures.
|Hom of Struct<'ut, 'un> * Struct<'vt, 'vn>
with       
    member x.Domain = 
        match x with
        |Morph(d, _, _)
        |Hom(d,_) -> d
  
    member x.CoDomain = 
        match x with
        |Morph(_, c, _)
        |Hom(_,c) -> c

    member x.Map = 
        match x with
        |Morph(_, _, m) -> m
        |Hom(_,_) -> failwith "The hom-set comprises all of the maps between 2 structures and is not an individual map."
    
    static member (*) (l:Morph<'ut, 'vt, 'un, 'vn>, r:Morph<'vt, 'wt, 'vn, 'wn>) =
        match l, r with
        | Morph(a, _, m), Morph(_, c, n) -> Morph(a, c, m >> n) 
        | _ -> failwith "Only individual morphisms are compatible for composition."
        
    /// Identity morphism.
    static member Id(s) = Morph(s, s, id)

type Morphisms<'ut, 'vt, 'un, 'vn, 'mn when 'ut : equality and 'vt : equality and 'un :> Number and 'vn :> Number and 'mn :> Number> = Array<'mn, Morph<'ut, 'vt, 'un, 'vn>>

/// A category of n morphisms between structures.
type Category<'ut, 'vt, 'un, 'vn, 'n when 'ut : equality and 'vt : equality and 'un :> Number and 'vn :> Number and 'n :> Number>(morphisms:Morphisms<'ut, 'vt, 'un, 'vn, 'n>) = 
    member val Morphisms = morphisms
    member val Source = morphisms.Map(fun m -> m.Domain)
    member val Target = morphisms.Map(fun m -> m.CoDomain)
    member x.Map = morphisms.Map(fun m -> m.Map)
    member inline x.Item(n, e:'ut) = x.Morphisms.[n].Map e
    member inline x.Item(e:'ut) = x.Morphisms._Array.[0].Map e

    new (l:Struct<'ut, 'un>, r:Struct<'vt, 'vn>, maps: Array<'n, Map<'ut, 'vt>>) = Category(maps.Map(fun m -> Morph(l, r, m)))

[<AutoOpen>]
module Category =
    let Morph(s, m) = Morph(s, s, m)
   
 


    