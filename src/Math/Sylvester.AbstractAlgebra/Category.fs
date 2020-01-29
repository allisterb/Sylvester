namespace Sylvester

open Sylvester.Arithmetic
open Sylvester.Collections

/// Morphism between 2 structures of type t and u.
type Morph<'ut, 'vt, 'un, 'vn, 'u, 'v when 'ut : equality and 'vt : equality and 'un :> Number and 'vn :> Number and 'u :> IStruct<'ut, 'un> and 'v :> IStruct<'vt, 'vn>> = 

/// Morphism defined by a map from elements of the set of one structure to another of the same type.
|Morph of 'u * 'v * Map<'ut, 'vt> 

/// hom-set of all morphisms between 2 structures of the same type.
|Hom of 'u * 'v
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
    
    static member (*) (l:Morph<'ut, 'vt, 'un, 'vn, 'u, 'v>, r:Morph<'vt, 'wt, 'vn, 'wn, 'v, 'w>) =
        match l, r with
        | Morph(a, _, m), Morph(_, c, n) -> Morph(a, c, m >> n) 
        | _ -> failwith "Only individual morphisms are compatible for composition."
        
    /// Identity morphism.
    static member Id(s) = Morph(s, s, id)

type Morphisms<'ut, 'vt, 'un, 'vn, 'u, 'v, 'mn when 'ut : equality and 'vt : equality and 'un :> Number and 'vn :> Number and 'u :> IStruct<'ut, 'un> and 'v :> IStruct<'vt, 'vn> and 'mn :> Number> = Array<'mn, Morph<'ut, 'vt, 'un, 'vn, 'u, 'v>>

/// A category of morphisms between 2 structures.
type Category<'ut, 'vt, 'un, 'vn, 'u, 'v, 'mn when 'ut : equality and 'vt : equality and 'un :> Number and 'vn :> Number and 'u :> IStruct<'ut, 'un> and 'v :> IStruct<'vt, 'vn> and 'mn :> Number>(morphisms:Morphisms<'ut, 'vt, 'un, 'vn, 'u, 'v, 'mn>) = 
    member val Morphisms = morphisms
    member val Source = morphisms.Map(fun m -> m.Domain)
    member val Target = morphisms.Map(fun m -> m.CoDomain)
    member x.Map = morphisms.Map(fun m -> m.Map)
    member inline x.Item(n, e:'ut) = x.Morphisms.[n].Map e
    
   
 


    