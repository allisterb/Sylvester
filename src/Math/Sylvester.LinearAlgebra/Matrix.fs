namespace Sylvester

open System

open Sylvester.Arithmetic
open Sylvester.Collections

[<StructuredFormatDisplay("{Display}")>]
type Matrix<'dim0, 'dim1, 't when 'dim0 :> Number and 'dim1 :> Number and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IComparable and 't :> IFormattable>
    ([<ParamArray>] data: 't list list) =
    let d0, d1 = number<'dim0>.IntVal, number<'dim1>.IntVal
    let array = array2D(data)
    do if array.Length <> d0 * d1 then failwithf "The initializing array has the dimensions %ix%i, not %ix%i." (array.GetLength(0)) (array.GetLength(1)) d0 d1
    //do if array.Length !=
    member val Dim0:'dim0 = number<'dim0>
    member val Dim1:'dim1 = number<'dim1>
    member val Array = Array2D<'dim0, 'dim1, 't>(array)
    member x.Display = sprintf "Matrix<%i, %i>" (x.Dim0.IntVal) (x.Dim1.IntVal)    
    //static member ofArray(data: 't [,]) = Vector(data)

type Mat<'dim0, 'dim1, 't when 'dim0 :> Number and 'dim1 :> Number and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable> =
    Matrix<'dim0, 'dim1, 't>

type Mat<'dim0, 'dim1 when 'dim0 :> Number and 'dim1 :> Number> = Matrix<'dim0, 'dim1, float>

