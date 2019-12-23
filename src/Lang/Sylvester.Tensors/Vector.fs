namespace Sylvester.Tensors

open System

open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10

[<AbstractClass>]
type Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1 when 'd10 :> Base10Digit and 'd9 :> Base10Digit 
                and 'd8 :> Base10Digit and 'd7 :> Base10Digit and 'd6 :> Base10Digit
                and 'd5 :> Base10Digit and 'd4 :> Base10Digit and 'd3 :> Base10Digit and 'd2 :> Base10Digit 
                and 'd1 :> Base10Digit
                and 't : struct and 't: (new: unit -> 't) and 't:> ValueType and 't :> IEquatable<'t> and 't :> IFormattable>() = 
    inherit Tensor<'t, _0, _0, _0, _0, _0, _0, _0, _0, _0, _1>()
   
    member val Dim0 = N10<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>()

    member x.Length = x.Dim0

    static member inline Vector = _true
 
    static member inline (!+)  (l:Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>) = l.Dim0 

    