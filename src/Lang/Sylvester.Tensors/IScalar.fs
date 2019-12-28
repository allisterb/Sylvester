namespace Sylvester.Tensors

open System

open Sylvester.Arithmetic.N10

type IScalar<'t when 't : struct and 't: (new: unit -> 't) and 't:> ValueType and 't :> IEquatable<'t> and 't :> IFormattable> =     
    inherit IPartialShape<zero>