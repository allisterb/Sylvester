namespace Sylvester.Tensors

open System

open Sylvester.Arithmetic

type IScalar<'t when 't : struct and 't: (new: unit -> 't) and 't:> ValueType and 't :> IEquatable<'t> and 't :> IFormattable> = 
    
    inherit IPartialShape<'t, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0>