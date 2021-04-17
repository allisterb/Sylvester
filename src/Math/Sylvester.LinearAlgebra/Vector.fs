namespace Sylvester

open System
open FSharp.Quotations

open MathNet.Numerics

open Sylvester.Arithmetic

[<StructuredFormatDisplay("{Display}")>]
type Vector<'t when 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>
    internal (e: Expr<'t array>, isSymbolic:bool) =
    let expr = expand e
    let data = e |> evaluate 
    do if data.Length = 0 then failwith "The length of a vector must one or greater."
    let vector = lazy LinearAlgebra.DenseVector.raw data  
    member val Expr = expr
    member val IsSymbolic = isSymbolic  
    member val _Array = data
    member val _Vector = vector
    member val Display = src expr
    interface IPartialShape<one> with
        member val Rank = Some 1 with get,set
        member val Dims = [| Convert.ToInt64(data.Length) |] |> Some with get,set
        member val Data = data :> Array with get, set
    new(d: Expr<'t list>) = Vector(<@ List.toArray %d @>, true)
    new([<ParamArray>] d:'t array) = Vector(<@ d @>, false)
    static member NumericOps = defaultLinearAlgebraNumericOps
    static member create([<ParamArray>] data: 't array) = Vector<'t>(data)
    static member fromMNVector(v: LinearAlgebra.Vector<'t>) = Vector<'t>.create(v.AsArray()) 
    static member (+)(l: Vector<'t>, r: Vector<'t>) = Vector<'t>.NumericOps.VecAdd l._Vector.Value r._Vector.Value 
    static member (-)(l: Vector<'t>, r: Vector<'t>) = Vector<'t>.NumericOps.VecSubtract l._Vector.Value r._Vector.Value
    static member (*)(l: Vector<'t>, r: Vector<'t>) = Vector<'t>.NumericOps.VecDotProduct l._Vector.Value r._Vector.Value

[<StructuredFormatDisplay("{Display}")>]
type Vector<'dim0, 't when 'dim0 :> Number and 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>
    (e: Expr<'t array>) =
    inherit Vector<'t>(e, true)
    let dim0 = number<'dim0>
    do if data.Length <> dim0.IntVal then failwithf "The initializing array has length %i instead of  %i." data.Length dim0.IntVal
    member val Dim0:'dim0 = dim0
    member val Display = sprintf "Vector<%i>\n%s" dim0.IntVal (base._Vector.Value.ToVectorString())
    interface IVector<'dim0> with member val Dim0 = dim0
    static member create([<ParamArray>] data: 't array) = Vector<'dim0, 't>(data)
    static member fromMNVector(v: LinearAlgebra.Vector<'t>) = Vector<'dim0, 't>.create(let c = v.AsArray() in if isNull(c) then v.ToArray() else c) 
    static member (+)(l: Vector<'dim0, 't>, r: Vector<'dim0, 't>) = l//._Vector + r._Vector.Value |> Vector<'dim0, 't>.fromMNVector
    static member Add (l:Vector<'n, 't>) (r:Vector<'n, 't>) = l + r
    //static member (-)(l: Vector<'dim0, 't>, r: Vector<'dim0, 't>) = l._Vector - r._Vector.Value |> Vector<'dim0, 't>.fromMNVector
    
type Vec<'dim0, 't when 'dim0 :> Number and 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable> =
    Vector<'dim0, 't>
type Vec<'dim0 when 'dim0 :> Number> = Vec<'dim0, real>
type VecF<'dim0 when 'dim0 :> Number> = Vec<'dim0, float32>
//type VecC<'dim0 when 'dim0 :> Number> = Vec<'dim0, complex>
