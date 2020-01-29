namespace Sylvester

open Sylvester.Arithmetic
open Sylvester.Collections

/// Morphism between 2 structures of the same type in universe U.
type Morph<'U, 's, 'n when 'U : equality and 'n :> Number and 's :> Struct<'U, 'n>> = 

/// Morphism defined by a map from elements of the set of one structure to another of the same type.
|Morph of 's * 's * Map<'U> 

/// hom-set of all morphisms between 2 structures of the same type.
|Hom of 's * 's
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
    
    static member (*) (l:Morph<'U, 's, 'n>, r:Morph<'U, 's, 'n>) =
        match l, r with
        | Morph(a, _, m), Morph(_, c, n) -> Morph(a, c, m >> n) 
        | _ -> failwith "Only individual morphisms are compatible for composition."
        
    /// Identity morphism.
    static member Id(s) = Morph(s, s, id)

type Morphisms<'U, 's, 'sn, 'mn when 'U : equality and 'sn :> Number and 's :> Struct<'U, 'sn> and 'mn :> Number> = Array<'mn, Morph<'U, 's, 'sn>>

type ICategory<'U, 'ob, 'sn when 'U: equality and 'sn :> Number and 'ob :> Struct<'U, 'sn>> =
    abstract member Morph: Morph<'U, 'ob, 'sn>

type Category<'U, 'ob, 'sn when 'U: equality and 'sn :> Number and 'ob :> Struct<'U, 'sn>>(morph:Morph<'U, 'ob, 'sn>) = 
    member val Morph = morph
    interface ICategory<'U, 'ob, 'sn> with 
        member val Morph = morph
    
    