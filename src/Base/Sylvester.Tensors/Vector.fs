namespace Sylvester.Tensors

open System
open MathNet.Numerics.LinearAlgebra

open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Arithmetic.Collections

type Vector<'t, 'd10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1 when 'd10 :> Base10Digit and 'd9 :> Base10Digit 
                and 'd8 :> Base10Digit and 'd7 :> Base10Digit and 'd6 :> Base10Digit
                and 'd5 :> Base10Digit and 'd4 :> Base10Digit and 'd3 :> Base10Digit and 'd2 :> Base10Digit 
                and 'd1 :> Base10Digit
                and 't : struct and 't: (new: unit -> 't) and 't:> ValueType and 't :> IEquatable<'t> and 't :> IFormattable>(n:N10<'d10, 'd9, 'd8, 'd7, 'd6, 'd5, 'd4, 'd3, 'd2, 'd1>, items:'t[]) = 
    inherit Tensor<'t, _0, _0, _0, _0, _0, _0, _0, _0, _0, _2>(two)
   
    member val Array = varray n items
    
    member val _Array = items

    member val _Vector = DenseVector.ofArray items

    member x.Dims = x.Array ^+^ VNil |> varrays

    member inline x.SetVal(i:'i, item: 't) = x.Array.SetVal(i, item)
    
    member inline x.Item(i:'i) = x.Array.[i]