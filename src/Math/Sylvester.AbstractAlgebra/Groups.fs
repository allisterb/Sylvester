namespace Sylvester.AbstractAlgebra

open System

open Sylvester.Arithmetic.N10
open Sylvester.Collections

/// Group of numbers with arithmetical properties or objects isomorphic to such numbers.
type Group<'t when 't : struct  and 't: comparison and 't: equality and 't :> IFormattable
    and 't : (static member (+): 't -> 't -> 't)
    and 't : (static member (-): 't -> 't -> 't)
    and 't : (static member (*): 't -> 't -> 't)
    and 't : (static member (/): 't -> 't -> 't)
    and 't : (static member Zero: 't)
    and 't : (static member One: 't)>(set: Set<'t>, op:BinaryOp<'t>) = 
    inherit Category<one, one, 't>((arrayOf1(Struct(set))),(arrayOf1(Morph(Struct(set), Struct(set), op))))
    member inline x.Set = x.Structures.[zero].Set
        