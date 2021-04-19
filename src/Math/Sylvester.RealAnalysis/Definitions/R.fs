namespace Sylvester

open Sylvester.Arithmetic

type R<'n when 'n :>Number>() = 
    inherit VectorSpace<'n, real, Vector<'n, real>>(Field.R, Vector.add, Vector.add)
    static member val Space = R<'n>() 
    static member val Field = R<'n>() :> IField<real>

[<AutoOpen>]
module R =
    let R<'n when 'n :> Number> = R<'n>.Space

    let euclid_dist (l:Vec<'n>) (r:Vec<'n>) = () 