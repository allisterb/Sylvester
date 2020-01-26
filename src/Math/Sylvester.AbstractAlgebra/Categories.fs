namespace Sylvester.AbstractAlgebra

open System

open Sylvester.Arithmetic
open Sylvester.Collections

/// Map or function between numbers of the same type.
type Map<'U when 'U : struct  and 'U: equality and 'U :> IFormattable> = 'U -> 'U 
   
/// Morphism between 2 sets of the same type.
type Morph<'U when 'U : struct  and 'U: equality and 'U :> IFormattable> = Morph of Set<'U> * Set<'U> * Map<'U> with   
    member x.Domain = let (Morph(d, _, _)) = x in d
    member x.CoDomain = let (Morph(_, c, _)) = x in c
    member x.Map = let (Morph(_, _, m)) = x in m
    static member inline Id(x) = Morph(x, x, id)  

    static member (*) (Morph(a, _, m), Morph(_, c, n))= Morph(a, c, m >> n)

type Morphisms<'n, 'U when 'n :> Number and 'U : struct  and 'U: equality and 'U :> IFormattable> = Array<'n, Morph<'U>>

/// A collection of sets and collection of morphisms in some universe U.
type ICategory<'U, 'obn, 'mn, 'ob, 'm  when 'U : struct  and 'U: equality and 'U :> IFormattable and 'obn :> Number and 'mn :> Number and 'ob :> Sets<'obn, 'U> and 'm  :> Morphisms<'mn, 'U>> = 
    abstract member Objects:'ob
    abstract member Morphisms:'m

/// Base implementation of a category.
type Category<'U, 'obn, 'mn, 'ob, 'm  when 'U : struct  and 'U: equality and 'U :> IFormattable and 'obn :> Number and 'mn :> Number and 'ob :> Sets<'obn, 'U> and 'm  :> Morphisms<'mn, 'U>>
    (objects: 'ob, morphisms: 'm) = 
    interface ICategory<'U, 'obn, 'mn, 'ob, 'm> with 
        member val Objects = objects
        member val Morphisms = morphisms