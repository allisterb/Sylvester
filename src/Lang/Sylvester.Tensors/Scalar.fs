namespace Sylvester.Tensors

open System

open Sylvester.Arithmetic

[<AbstractClass>]
[<StructuredFormatDisplay("{Val}")>]
type Scalar<'t when 't : struct and 't: (new: unit -> 't) and 't:> ValueType and 't :> IEquatable<'t> and 't :> IFormattable>() = 
    inherit Tensor<'t, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0>()