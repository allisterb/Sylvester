namespace Sylvester

open System

open MathNet.Numerics
open Sylvester.Arithmetic
open Sylvester.Collections

[<StructuredFormatDisplay("{Display}")>]
type Vector<'t when 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>
    ([<ParamArray>] data: 't array) =
    do if data.Length = 0 then failwith "The length of a vector must one or greater."
    member val _Array = data
    member val _Vector = LinearAlgebra.DenseVector.raw data     
    interface IPartialShape<one> with
        member val Rank = Some 1 with get,set
        member val Dims = [| Convert.ToInt64(data.Length) |] |> Some with get,set

    static member Ops = defaultLinearAlgebraOps
    static member create([<ParamArray>] data: 't array) = Vector<'t>(data)
    static member fromMNVector(v: LinearAlgebra.Vector<'t>) = Vector<'t>.create(v.AsArray()) 
    static member (+)(l: Vector<'t>, r: Vector<'t>) = Vector<'t>.Ops.VecAdd l._Vector r._Vector 
    static member (-)(l: Vector<'t>, r: Vector<'t>) = Vector<'t>.Ops.VecSubtract l._Vector r._Vector
    static member (*)(l: Vector<'t>, r: Vector<'t>) = Vector<'t>.Ops.VecDotProduct l._Vector r._Vector

[<StructuredFormatDisplay("{Display}")>]
type Vector<'dim0, 't when 'dim0 :> Number and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>
    ([<ParamArray>] data: 't array) =
    inherit Vector<'t>(data)
    let dim0 = number<'dim0>
    do if data.Length <> dim0.IntVal then failwithf "The initializing array has length %i instead of  %i." data.Length dim0.IntVal
    member val Dim0:'dim0 = dim0
    member val Display = sprintf "Vector<%i>\n%s" dim0.IntVal (base._Vector.ToVectorString())
    interface IVector<'dim0> with member val Dim0 = dim0
    
    static member create([<ParamArray>] data: 't array) = Vector<'dim0, 't>(data)
    static member fromMNVector(v: LinearAlgebra.Vector<'t>) = Vector<'dim0, 't>.create(let c = v.AsArray() in if isNull(c) then v.ToArray() else c) 
    static member (+)(l: Vector<'dim0, 't>, r: Vector<'dim0, 't>) = l._Vector + r._Vector |> Vector<'dim0, 't>.fromMNVector
    static member (-)(l: Vector<'dim0, 't>, r: Vector<'dim0, 't>) = l._Vector - r._Vector |> Vector<'dim0, 't>.fromMNVector
    
type Vec<'dim0, 't when 'dim0 :> Number and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable> =
    Vector<'dim0, 't>
type Vec<'dim0 when 'dim0 :> Number> = Vec<'dim0, R>
type VecF<'dim0 when 'dim0 :> Number> = Vec<'dim0, float32>