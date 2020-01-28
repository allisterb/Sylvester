namespace Sylvester

open Sylvester.Arithmetic

/// Morphism between 2 structures of the same type in universe U.
type Morph<'U, 's, 'n when 'U : equality and 'n :> Number and 's :> Struct<'U, 'n>> = 

/// Morphism defined by a map from elements of the set of one structure to another of the same type.
|Morph of 's * 's * Map<'U> 

/// hom-set of all morphisms between 2 structures of the same type.
|Hom of 's * 's

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
        |Hom(_,_) -> failwith "The hom-set comprises all of the maps between 2 structures and is not an individual map."
    
    static member (*) (l:Morph<'U, 's, 'n>, r:Morph<'U, 's, 'n>) =
        match l, r with
        | Morph(a, _, m), Morph(_, c, n) -> Morph(a, c, m >> n) 
        | _ -> failwith "Only individual morphisms are compatible for composition."
        
    /// Identity morphism.
    static member Id(s) = Morph(s, s, id)

type ICategory<'U, 'ob, 'mn when 'U: equality and 'ob :> Struct<'U, 'mn> and 'mn :> Number> =
    abstract member Morph: Morph<'U, 'ob, 'mn>

type Category<'U, 'ob, 'mn when 'U: equality and 'ob :> Struct<'U, 'mn> and 'mn :> Number>(morph:Morph<'U, 'ob, 'mn>) = 
    member val Morph = morph
    interface ICategory<'U, 'ob, 'mn> with 
        member val Morph = morph
    