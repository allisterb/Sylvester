namespace Sylvester

open System

open FSharp.Quotations

open Sylvester.Arithmetic

open MathNet.Numerics

[<StructuredFormatDisplay("{Display}")>]
type Matrix<'t when 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>
    internal (data: 't array array, ?expr: Expr<'t list list>) =
    do if expr.IsNone && data |> Array.forall (fun a -> a.Length = data.[0].Length) |> not then failwith "The length of each column in a matrix must be the same."
    let matrix = lazy LinearAlgebra.DenseMatrix.ofColumnArrays data
    
    member val _Array = data
    member val _Matrix = matrix
    member val Expr = expr
    member val IsSymbolic = expr.IsSome
    member val Display = "foo" 
        //match typeof<'t> with
        //| LinearAlgebraNumericOpType _ -> matrix.Value.ToString()  
        //| _ -> data.ToString()     
    interface IPartialShape<two> with
        member val Rank = Some 2 with get,set
        member val Dims = if data.Length <> 0 then [| data.[0].LongLength; data.LongLength |] |> Some else None with get,set
        member val Data = data :> Array with get, set
    new([<ParamArray>] data: 't list array) =
        Matrix<'t>(data |> Array.map (fun a -> a |> List.toArray)) 
    new(x: Expr<'t list list>) = 
        let values = 
            match expand x with
            | WithoutVariables _ -> x |> expand_list_values<'t> |> List.map List.toArray |> List.toArray
            | _ -> x |> expand_list_values''<'t> |> List.map List.toArray |> List.toArray
        Matrix<'t>(values, x)

    member x.toDouble() = x._Array |> Array.map(fun ar -> ar |> Array.map (fun a -> Convert.ToDouble a)) |> Matrix
    member x.toInt32() = x._Array |> Array.map(fun ar -> ar |> Array.map (fun a -> Convert.ToInt32 a)) |> Matrix
    member x.toInt64() = x._Array |> Array.map(fun ar -> ar |> Array.map (fun a -> Convert.ToInt64 a)) |> Matrix
    member x.toRational() = x._Array |> Array.map(fun ar -> ar |> Array.map (fun a -> Rational(Convert.ToDouble(a)))) |> Matrix
    
    static member Ops = defaultLinearAlgebraOps
    static member create(x: Array) = Matrix(x :?> 't [] [])
    static member create(x:_Matrix<'t>) = Matrix<'t>(let a = x.AsColumnArrays() in if not(isNull (a)) then a else x.ToColumnArrays()) 
    static member internal toDouble(m:Matrix<'t>) = m._Array |> Array.map(fun ar -> ar |> Array.map (fun a -> Convert.ToDouble a)) |> Matrix.create
    static member internal toInt32(m:Matrix<'t>) = m._Array |> Array.map(fun ar -> ar |> Array.map (fun a -> Convert.ToInt32 a)) |> Matrix.create
    static member internal toInt64(m:Matrix<'t>) = m._Array |> Array.map(fun ar -> ar |> Array.map (fun a -> Convert.ToInt64 a)) |> Matrix.create
    static member internal toRational(m:Matrix<'t>) = m._Array |> Array.map(fun ar -> ar |> Array.map (fun a -> Rational(Convert.ToDouble(a)))) |> Matrix.create
    
    static member (+)(l : Matrix<'t>, r : Matrix<'t>) :Matrix<'t>= 
        match typeof<'t> with
        | LinearAlgebraNumericOpType _ -> let res = Matrix<'t>.Ops.MatAdd l._Matrix.Value r._Matrix.Value in res |> Matrix.create
        | Int32Type _ -> let res = l.toDouble() + r.toDouble() in res |> Matrix.toInt32  
        | Int64Type _ -> let res = l.toDouble() + r.toDouble() in res |> Matrix.toInt64
        | RationalType _ -> let res = l.toDouble() + r.toDouble() in res |> Matrix.toRational
        | t -> failwithf "Matrix addition for type %s is not supported." t.Name
        
[<StructuredFormatDisplay("{Display}")>]
type Matrix<'dim0, 'dim1, 't when 'dim0 :> Number and 'dim1 :> Number and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IComparable and 't :> IFormattable>
    (data: 't array array, ?expr: Expr<'t list list>) =
    inherit Matrix<'t>(data, ?expr = expr) 
    let d0, d1 = number<'dim0>.IntVal, number<'dim1>.IntVal
    do if base._Array.Length > 0 && (base._Array.Length <> d1  || (base._Array |> Array.forall (fun a -> a.Length = d0) |> not)) then failwithf "The initializing array does not have the dimensions %ix%i." d1 d0
    member val Dim0:'dim0 = number<'dim0>
    member val Dim1:'dim1 = number<'dim1>    
    member x.toDouble() = x._Array |> Array.map(fun ar -> ar |> Array.map (fun a -> Convert.ToDouble a)) |> Matrix<'dim0,'dim1,_>
    member x.toInt32() = x._Array |> Array.map(fun ar -> ar |> Array.map (fun a -> Convert.ToInt32 a)) |> Matrix<'dim0,'dim1,_>
    member x.toInt64() = x._Array |> Array.map(fun ar -> ar |> Array.map (fun a -> Convert.ToInt64 a)) |> Matrix<'dim0,'dim1,_>
    member x.toRational() = x._Array |> Array.map(fun ar -> ar |> Array.map (fun a -> Rational(Convert.ToDouble(a)))) |> Matrix<'dim0,'dim1,_>
    interface IMatrix<'dim0, 'dim1>
    new (m:Matrix<'t>) = Matrix<'dim0, 'dim1, 't>(m._Array)
    new([<ParamArray>] data: 't list array) =
        Matrix<'dim0, 'dim1, 't>(data |> Array.map (fun a -> a |> List.toArray)) 
    new(x: Expr<'t list list>) = 
        let m = Matrix<'t>(x) in Matrix<'dim0, 'dim1, 't>(m)
        // let values = x |> expand_list_values'<'t> |> List.map List.toArray |> List.toArray in
        // Matrix<'dim0,'dim1, 't>(values, x)
    static member create(data: Array) = Matrix<'dim0,'dim1,'t>(data :?> 't [] [])
    static member create(m: _Matrix<'t>) = Matrix<'dim0, 'dim1, 't>.create(let a = m.AsRowArrays() in if isNull a then m.ToColumnArrays() else a) 
    static member create(m:Matrix<'t>) = Matrix<'dim0, 'dim1, 't>(m._Array)  
    static member op_Explicit(m:Matrix<'dim0, 'dim1, 't>) = m._Array |> Array.map(fun ar -> ar |> Array.map (fun a -> Convert.ToDouble a)) |> Matrix<_,_,_>
    static member (+)(l : Matrix<'dim0, 'dim1, 't>, r : Matrix<'dim0, 'dim1, 't>) = let res = (l:>Matrix<'t>) + (r :> Matrix<'t>) in Matrix<'dim0, 'dim1, 't>.create res._Array  

type Mat<'dim0, 'dim1, 't when 'dim0 :> Number and 'dim1 :> Number and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable> =
    Matrix<'dim0, 'dim1, 't>
type Mat<'dim0, 'dim1 when 'dim0 :> Number and 'dim1 :> Number> = Matrix<'dim0, 'dim1, real>
type MatF<'dim0, 'dim1 when 'dim0 :> Number and 'dim1 :> Number> = Matrix<'dim0, 'dim1, real32>