namespace Sylvester

open Sylvester.Arithmetic
open Sylvester.Collections

/// Morphism between 2 structures of type u, v.
type Morph<'t, 's, 'n when 't : equality and 'n :> Number and 's :> Struct<'t, 'n>> = 

/// Morphism defined by a map from elements of the set of one structure to another of the same type.
|Morph of 's * 's * Map<'t> 

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
    
    static member (*) (l:Morph<'t, 's, 'n>, r:Morph<'t, 's, 'n>) =
        match l, r with
        | Morph(a, _, m), Morph(_, c, n) -> Morph(a, c, m >> n) 
        | _ -> failwith "Only individual morphisms are compatible for composition."
        
    /// Identity morphism.
    static member Id(s) = Morph(s, s, id)

type Morphisms<'t, 's, 'sn, 'mn when 't : equality and 'sn :> Number and 's :> Struct<'t, 'sn> and 'mn :> Number> = Array<'mn, Morph<'t, 's, 'sn>>

type ICategory<'t, 'ob, 'sn when 't: equality and 'sn :> Number and 'ob :> Struct<'t, 'sn>> =
    abstract member Morph: Morph<'t, 'ob, 'sn>

type Category<'t, 'ob, 'sn when 't: equality and 'sn :> Number and 'ob :> Struct<'t, 'sn>>(morph:Morph<'t, 'ob, 'sn>) = 
    member val Morph = morph
    interface ICategory<'t, 'ob, 'sn> with 
        member val Morph = morph
    
    