namespace Sylvester.Tensors

open System
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Arithmetic.Collections

[<StructuredFormatDisplay("{Val}")>]
type Scalar<'t when 't : struct and 't: (new: unit -> 't) and 't:> ValueType and 't :> IEquatable<'t> and 't :> IFormattable>(x:'t) = 
    inherit Tensor<'t, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0>(zero)

    member val Val = x
    member inline x.Dims = VNil
