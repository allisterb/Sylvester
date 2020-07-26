namespace Sylvester

open System

open Sylvester.Arithmetic

open MathNet.Numerics

[<StructuredFormatDisplay("{Display}")>]
type Matrix<'t when 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable>
    ([<ParamArray>] data: 't array array) =
    do if data.Length = 0 || data |> Array.forall (fun a -> a.Length <> 0) |> not then failwith "The number of rows in a matrix must be one or greater."
    let matrix = LinearAlgebra.DenseMatrix.ofRowArrays data
    member val _Array = data
    member val _Matrix = matrix
    member val Display = sprintf "%A" matrix
    interface IPartialShape<two> with
        member val Rank = Some 2 with get,set
        member val Dims = [| Convert.ToInt64(matrix.RowCount); Convert.ToInt64(matrix.ColumnCount) |] |> Some with get,set
    
    static member Ops = defaultLinearAlgebraOps
    static member (+)(l : Matrix<'t>, r : Matrix<'t>) = Matrix<'t>.Ops.MatAdd l._Matrix r._Matrix 
    static member (-)(l : Matrix<'t>, r : Matrix<'t>) = Matrix<'t>.Ops.MatSubtract l._Matrix r._Matrix
    static member (*)(l : Matrix<'t>, r : Matrix<'t>) = Matrix<'t>.Ops.MatMultiply l._Matrix r._Matrix 

[<StructuredFormatDisplay("{Display}")>]
type Matrix<'dim0, 'dim1, 't when 'dim0 :> Number and 'dim1 :> Number and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IComparable and 't :> IFormattable>
    ([<ParamArray>] data: 't array array) =
    inherit Matrix<'t>(data)
    let d0, d1 = number<'dim0>.IntVal, number<'dim1>.IntVal
    do if base._Array.Length <> d0  || (base._Array |> Array.forall (fun a -> a.Length = d1) |> not) then failwithf "The initializing array does not have the dimensions %ix%i."d0 d1
    member val Dim0:'dim0 = number<'dim0>
    member val Dim1:'dim1 = number<'dim1>
    member val Display = 
        sprintf "Matrix<%i,%i>\n%s" base._Matrix.RowCount base._Matrix.ColumnCount (base._Matrix.ToMatrixString())
    interface IMatrix<'dim0, 'dim1>
    new ([<ParamArray>] data: 't list array) = 
        let array = data |> Array.map (fun a -> Array.ofList a) in
        Matrix<'dim0, 'dim1, 't>(array)
    
    static member create([<ParamArray>] data: 't array array) = Matrix<'dim0,'dim1,'t>(data)
    static member fromMNMatrix(m: LinearAlgebra.Matrix<'t>) = Matrix<'dim0, 'dim1, 't>.create(let a = m.AsRowArrays() in if isNull a then m.ToRowArrays() else a) 
    static member (+)(l : Matrix<'n, 'm, 't>, r : Matrix<'n, 'm, 't>) = l._Matrix + r._Matrix |> Matrix<'n, 'm, 't>.fromMNMatrix
    static member (-)(l : Matrix<'n, 'm, 't>, r : Matrix<'n, 'm, 't>) = l._Matrix - r._Matrix |> Matrix<'n, 'm, 't>.fromMNMatrix
    static member (*)(l : Matrix<'n, 'm, 't>, r : Matrix<'m, 'p, 't>) =  l._Matrix * r._Matrix |> Matrix<'n, 'p, 't>.fromMNMatrix

type Mat<'dim0, 'dim1, 't when 'dim0 :> Number and 'dim1 :> Number and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable and 't :> IComparable> =
    Matrix<'dim0, 'dim1, 't>
type Mat<'dim0, 'dim1 when 'dim0 :> Number and 'dim1 :> Number> = Matrix<'dim0, 'dim1, R>
type MatF<'dim0, 'dim1 when 'dim0 :> Number and 'dim1 :> Number> = Matrix<'dim0, 'dim1, float32>