namespace Sylvester

open System
open FSharp.Quotations

open MathNet.Numerics

open Sylvester.Arithmetic

[<StructuredFormatDisplay("{Display}")>]
type Vector<'t when 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>
    internal(e: Expr<'t> array, d: 't array, isSymbolic:bool) = 
    do if e.Length = 0 then failwith "The length of a vector must one or greater."
    let expr = e  |> Array.map expand'<'t, 't>
    let data = d      
    let vector = lazy LinearAlgebra.DenseVector.raw data  
    member val _Vector = vector
    member val Array = data
    member val Expr = expr
    member val ExprVars = expr |> Array.map (get_vars >> List.toArray) |> Array.concat
    member val Expr' = Array.map MathNetExpr.fromQuotation expr
    member val IsSymbolic = isSymbolic  
    member val Display = expr
    interface IPartialShape<one> with
        member val Rank = Some 1 with get,set
        member val Dims = [| Convert.ToInt64(data.Length) |] |> Some with get,set
        member val Data = data :> Array with get, set
    new([<ParamArray>] v:'t array) = let expr = v |> Array.map(fun e -> <@ e @>) in Vector<'t>(expr, v, false)
    new(v: Expr<'t list>) = let expr = v |> expand_list' |> List.toArray in Vector<'t>(expr, (v |> evaluate |> List.toArray), true)
    new(d:'t list) = Vector<'t>(List.toArray d)
    static member create([<ParamArray>] data: 't array) = Vector<'t>(data)

[<StructuredFormatDisplay("{Display}")>]
type Vector<'dim0, 't when 'dim0 :> Number and 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>
    internal (e: Expr<'t> array, d: 't array, isSymbolic:bool) =
    inherit Vector<'t>(e, d, isSymbolic)
    let dim0 = number<'dim0>
    do if d.Length <> dim0.IntVal then failwithf "The initializing array has length %i instead of %i." d.Length dim0.IntVal
    member val Dim0:'dim0 = dim0
    member val Display = base.Display
    interface IVector<'dim0> with member val Dim0 = dim0
    new([<ParamArray>] v:'t array) = let expr = v |> Array.map(fun e -> <@ e @>) in Vector<'dim0, 't>(expr, v, false)
    new(v: Expr<'t list>) = let expr = v |> expand_list' |> List.toArray in Vector<'dim0, 't>(expr, (v |> evaluate |> List.toArray), true)
    new(d:'t list) = Vector<'dim0, 't>(List.toArray d)
    static member create([<ParamArray>] data: 't array) = Vector<'dim0, 't>(data)
    
    static member (+) (l: Vector<'dim0, 't>, r: Vector<'dim0, 't>) = 
        let e = defaultLinearAlgebraSymbolicOps.Add l.Expr r.Expr 
        let d = let v = defaultLinearAlgebraNumericOps.Add l._Vector.Value r._Vector.Value in v.AsArray()
        Vector<'dim0, 't>(e, d, (l.IsSymbolic || r.IsSymbolic))
    
    static member (-) (l: Vector<'dim0, 't>, r: Vector<'dim0, 't>) = 
        let e = defaultLinearAlgebraSymbolicOps.Subtract l.Expr r.Expr 
        let d = let v = defaultLinearAlgebraNumericOps.Subtract l._Vector.Value r._Vector.Value in v.AsArray()
        Vector<'dim0, 't>(e, d, (l.IsSymbolic || r.IsSymbolic))

    static member (<.>) (l: Vector<'dim0, 't>, r: Vector<'dim0, 't>) = 
        let e = defaultLinearAlgebraSymbolicOps.InnerProduct l.Expr r.Expr 
        let d = defaultLinearAlgebraNumericOps.InnerProduct l._Vector.Value r._Vector.Value
        if l.IsSymbolic || r.IsSymbolic then e else expand'<'t, 't> <@ d @>

type Vec<'dim0, 't when 'dim0 :> Number and 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable> =
    Vector<'dim0, 't>
type Vec<'dim0 when 'dim0 :> Number> = Vec<'dim0, real>
type VecF<'dim0 when 'dim0 :> Number> = Vec<'dim0, float32>

module Vector =
    let add (l:Vector<'n, 't>) (r:Vector<'n, 't>) = l + r
