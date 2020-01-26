namespace Sylvester.AbstractAlgebra

open System

open Sylvester.Arithmetic
open Sylvester.Collections

/// Morphism between 2 structures of the same type in Universe U.
type Morph<'U, 'n when 'U : equality and 'n :> Number> = 

/// Single morphism defined by a map or function from one structure to another.
|Morph of Struct<'U, 'n> * Struct<'U, 'n> * Map<Struct<'U, 'n>>

/// Hom-set of all morphisms between 2 structures
|Hom of Struct<'U, 'n> * Struct<'U, 'n>

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
        |Hom(_,_) -> failwith "The hom-set comprises all of the maps between 2 sets and is not an individual map."
    
    static member (*) (l:Morph<'U, 'n>, r:Morph<'U, 'n>) =
        match l, r with
        | Morph(a, _, m), Morph(_, c, n) -> Morph(a, c, m >> n) 
        | _ -> failwith "Only individual morphisms are compatible for composition."
        
    /// Identity morphism
    static member Id(s) = Morph(s, s, id)

type Category<'U, 'mn when 'U: equality and 'mn :> Number>(s:Struct<'U, _>, m:Array<'mn, Morph<'U, _>>) = 
    member val Obj = s
    member val Morphpisms = m

