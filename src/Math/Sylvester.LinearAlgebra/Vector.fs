﻿namespace Sylvester

open System

open MathNet.Numerics
open Sylvester.Arithmetic
open Sylvester.Collections

type MNVector<'t when 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable> = LinearAlgebra.Vector<'t>

[<StructuredFormatDisplay("{Display}")>]
type Vector<'t when 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>
    ([<ParamArray>] data: 't array) =
    do if data.Length = 0 then failwith "The length of a vector must one or greater."
    interface IPartialShape<one> with
        member val Rank = Some 1 with get,set
        member val Dims = [| Convert.ToInt64(data.Length) |] |> Some with get,set
    member val _Array = data
    member val _Vector = LinearAlgebra.DenseVector.raw data
    member val Display = sprintf "Vector<%i>" (data.Length)     
    
    static member create([<ParamArray>] data: 't array) = Vector<'t>(data)
    static member fromArray (data: 't array) = Vector<'t>(data)
    static member fromMNVector(v: LinearAlgebra.Vector<'t>) = Vector<'t>.fromArray(v.AsArray()) 

[<StructuredFormatDisplay("{Display}")>]
type Vector<'dim0, 't when 'dim0 :> Number and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>
    ([<ParamArray>] data: 't array) =
    inherit Vector<'t>(data)
    let dim0 = number<'dim0>
    do if data.Length <> dim0.IntVal then failwithf "The initializing array has length %i instead of  %i." data.Length dim0.IntVal
    let array = Array<'dim0, 't>(data)
    member val Dim0:'dim0 = dim0
    member val Array = array
    interface IVector<'dim0> with
        member val Dim0 = dim0

    static member create([<ParamArray>] data: 't array) = Vector<'n, 't>(data)
    static member fromArray (data: 't array) = Vector<'dim0, 't>(data)
    static member fromMNVector(v: LinearAlgebra.Vector<'t>) = Vector<'dim0, 't>.fromArray(v.AsArray()) 
    static member (+)(l : Vector<'n, 't>, r : Vector<'n, 't> when 'n :> Number) : Vector<'n, 't> = Vector<'n, 't>.fromMNVector(l._Vector.Add(r._Vector))
    static member (-)(l : Vector<'n, 't>, r : Vector<'n, 't> when 'n :> Number) : Vector<'n, 't> = Vector<'n, 't>.fromMNVector(l._Vector.Subtract(r._Vector))
    static member (*)(l : Vector<'n, 't>, r : Vector<'n, 't> when 'n :> Number) : Vector<'n, 't> = Vector<'n, 't>.fromMNVector(l._Vector.PointwiseMultiply(r._Vector))

type Vec<'dim0, 't when 'dim0 :> Number and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable> =
    Vector<'dim0, 't>

type Vec<'dim0 when 'dim0 :> Number> = Vec<'dim0, R>