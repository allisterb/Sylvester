namespace Sylvester

open System
open FSharp.Quotations

open MathNet.Numerics

open Sylvester.Arithmetic

[<StructuredFormatDisplay("{Display}")>]
type Vector<'t when 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>
    internal(e: Expr<'t> array) = 
    do if e.Length = 0 then failwith "The length of a vector must one or greater."
    let expr = e  |> Array.map expand'<'t, 't>
    member val Expr = expr
    member val ExprVars = expr |> Array.map (get_vars >> List.toArray) |> Array.concat
    member val Expr' = Array.map MathNetExpr.fromQuotation expr
    member val Display = expr
    member x.AsNumeric() = 
        let t= typeof<'t>
        match t with
        | LinearAlgebraNumericOpType -> expr |> Array.map evaluate |> LinearAlgebra.DenseVector.raw
        | _ -> failwithf "The type %A is not compatible with numeric linear algebra operations." t
    interface IPartialShape<one> with
        member val Rank = Some 1 with get,set
        member val Dims = [| Convert.ToInt64(e.Length) |] |> Some with get,set
    new([<ParamArray>] v:'t array) = let expr = v |> Array.map(fun e -> <@ e @>) in Vector<'t>(expr)
    new(v: Expr<'t list>) = let expr = v |> expand_list' |> List.toArray in Vector<'t>(expr)
    new(d:'t list) = Vector<'t>(List.toArray d)
    static member create([<ParamArray>] data: 't array) = Vector<'t>(data)

[<StructuredFormatDisplay("{Display}")>]
type Vector<'dim0, 't when 'dim0 :> Number and 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>
    internal (e: Expr<'t> array) =
    inherit Vector<'t>(e)
    let dim0 = number<'dim0>
    do if e.Length <> dim0.IntVal then failwithf "The initializing array has length %i instead of %i." e.Length dim0.IntVal
    member val Dim0:'dim0 = dim0
    member val Display = base.Display
    interface IVector<'dim0> with member val Dim0 = dim0
    new([<ParamArray>] v:'t array) = let expr = v |> Array.map(fun e -> <@ e @>) in Vector<'dim0, 't>(expr)
    new(v: Expr<'t list>) = let expr = v |> expand_list' |> List.toArray in Vector<'dim0, 't>(expr)
    new(d:'t list) = Vector<'dim0, 't>(List.toArray d)
    static member create([<ParamArray>] data: 't array) = Vector<'dim0, 't>(data)
    
    static member (+) (l: Vector<'dim0, 't>, r: Vector<'dim0, 't>) = 
        let e = defaultLinearAlgebraSymbolicOps.Add l.Expr r.Expr in Vector<'dim0, 't>(e)
    
    static member (-) (l: Vector<'dim0, 't>, r: Vector<'dim0, 't>) = 
        let e = defaultLinearAlgebraSymbolicOps.Subtract l.Expr r.Expr in Vector<'dim0, 't>(e)

    static member (*) (l: Vector<'dim0, 't>, r: Vector<'dim0, 't>) = 
        let e = defaultLinearAlgebraSymbolicOps.InnerProduct l.Expr r.Expr in Scalar<'t> e

type Vec<'dim0, 't when 'dim0 :> Number and 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable> =
    Vector<'dim0, 't>
type Vec<'dim0 when 'dim0 :> Number> = Vec<'dim0, real>
type VecF<'dim0 when 'dim0 :> Number> = Vec<'dim0, float32>

module Vector =
    let add (l:Vector<'n, 't>) (r:Vector<'n, 't>) = l + r
    let sub (l:Vector<'n, 't>) (r:Vector<'n, 't>) = l - r