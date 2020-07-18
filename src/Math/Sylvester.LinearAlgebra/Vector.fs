namespace Sylvester

open System

open MathNet.Numerics
open Sylvester.Arithmetic
open Sylvester.Collections

[<StructuredFormatDisplay("{Display}")>]
type Vector<'dim0, 't when 'dim0 :> Number and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>
    ([<ParamArray>] data: 't array) =
    let dim0 = number<'dim0>
    do if data.Length <> dim0.IntVal then failwithf "The initializing array has length %i instead of  %i." data.Length dim0.IntVal
    let array = Array<'dim0, 't>(data)
    member val Dim0:'dim0 = dim0
    member val Array = array
    member val _Array = array._Array
    member val _Vector = LinearAlgebra.DenseVector.raw data
    member val Display = sprintf "Vector<%i>" (dim0.IntVal)     
    static member ofArray(data: 't array) = Vector(data)

type Vec<'dim0, 't when 'dim0 :> Number and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable> =
    Vector<'dim0, 't>

type Vec<'dim0 when 'dim0 :> Number> = Vec<'dim0, float>

