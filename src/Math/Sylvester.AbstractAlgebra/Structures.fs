namespace Sylvester.AbstractAlgebra

open System
open System.Collections
open System.Collections.Generic
open Microsoft.FSharp.Collections

///Binary op between numbers or objects isomorphic to numbers
type BinaryOp<'t when 't : struct  and 't: comparison and 't: equality and 't :> IFormattable
    and 't : (static member (+): 't -> 't -> 't)
    and 't : (static member (-): 't -> 't -> 't)
    and 't : (static member (*): 't -> 't -> 't)
    and 't : (static member (/): 't -> 't -> 't)
    and 't : (static member Zero: 't)> = 't -> 't -> 't

/// Group of numbers or objects isomorphic to numbers
type Group<'t when 't : struct  and 't: comparison and 't: equality and 't :> IFormattable
    and 't : (static member (+): 't -> 't -> 't)
    and 't : (static member (-): 't -> 't -> 't)
    and 't : (static member (*): 't -> 't -> 't)
    and 't : (static member (/): 't -> 't -> 't)
    and 't : (static member Zero: 't)> = Field of R<'t> * BinaryOp<'t>


/// Field of numbers or objects isomorphic to numbers
type Field<'t when 't : struct  and 't: comparison and 't: equality and 't :> IFormattable
    and 't : (static member (+): 't -> 't -> 't)
    and 't : (static member (-): 't -> 't -> 't)
    and 't : (static member (*): 't -> 't -> 't)
    and 't : (static member (/): 't -> 't -> 't)
    and 't : (static member Zero: 't)> = Field of R<'t> * BinaryOp<'t> * BinaryOp<'t>


[<AutoOpen>]
module Fields = 
    let R = Field(Real, (+), (*))
