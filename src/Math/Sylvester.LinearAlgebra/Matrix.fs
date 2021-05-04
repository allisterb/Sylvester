namespace Sylvester

open System
open FSharp.Quotations

open MathNet.Numerics
open Sylvester.Arithmetic
open Dimension

[<StructuredFormatDisplay("{Display}")>]
type Matrix<'t when 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable>
    internal(e: Expr<'t> array array) = 
    do if e |> Array.forall (fun a -> a.Length = e.[0].Length) |> not then failwith "The length of each column in a matrix must be the same."
    let expr = e  |> Array.map (Array.map expand''<'t>)
    let expr' = Array.map (Array.map MathNetExpr.fromQuotation) expr
    (* Transposes rectangular matrices *)
    let transpose matrix =
        let rec fetch_column acc (matr:(Expr<'t> list list)) = (* Makes a column list from a row list *)
            if matr.Head.Length = 0 then (List.rev acc) (* Stop *)
            else fetch_column
                    ([for row in matr -> row.Head]::acc) (* Fetches the first item from each row *)
                    (List.map (fun row -> match row with [] -> [] | h::t -> t) matr)
        fetch_column [] matrix 
    let exprT = expr |> Array.map(Array.toList) |> Array.toList |> transpose |> List.map(List.toArray) |> List.toArray
    member val Expr = expr
    member val ExprVars = expr |> Array.map (Array.map(get_vars >>List.toArray)) |> Array.concat
    member val Expr' = expr'
    member val Display = ""
    member val ExprT = exprT
    member val RowVectors = expr |> Array.map Vector<'t>
    member val ColumnVectors = exprT |> Array.map Vector<'t>
    member x.Item(i:int, j:int) = expr.[i].[j]
    member x.AsNumeric() = 
        let t = typeof<'t>
        match t with
        | LinearAlgebraNumericOpType -> expr |> Array.map (Array.map evaluate) |> LinearAlgebra.DenseMatrix.ofRowArrays
        | _ -> failwithf "The type %A is not compatible with numeric linear algebra operations." t
    interface IPartialShape<one> with
        member val Rank = Some 1 with get,set
        member val Dims = [| Convert.ToInt64(e.Length) |] |> Some with get,set
    new([<ParamArray>] v:'t array array) = let expr = v |> Array.map(Array.map(fun e -> <@ e @>)) in Matrix<'t>(expr)
    new(v: Expr<'t list list>) = let expr = v |> expand_lists' |> List.map(List.toArray) |> List.toArray in Matrix<'t>(expr)
    new(d:'t list list) = Matrix<'t>((List.map(List.toArray) >> List.toArray) d)
    static member create([<ParamArray>] data: 't array array) = Matrix<'t>(data)

[<StructuredFormatDisplay("{Display}")>]
type Matrix<'dim0, 'dim1, 't when 'dim0 :> Number and 'dim1 :> Number and 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable>
    internal (e: Expr<'t> array array) =
    inherit Matrix<'t>(e)
    let dim0 = number<'dim0>
    let dim1 = number<'dim1>
    do if e.Length <> dim0.IntVal || e.[0].Length <> dim1.IntVal then failwithf "The initializing array has dimensions [%i][%i] instead of [%i][%i]." e.Length e.[0].Length dim0.IntVal dim1.IntVal
    member val Dim0:'dim0 = dim0
    member val Dim1:'dim1 = dim1
    member val Display = base.Display
    member x.ColumnVectors = x.ExprT |> Array.map Vector<'dim0, 't>
    member x.RowVectors = x.Expr |> Array.map Vector<'dim1, 't>
    member x.Item(i: int) = x.RowVectors.[i]
    interface IMatrix<'dim0, 'dim1> with 
        member val Dim0 = dim0
        member val Dim1 = dim1
    new([<ParamArray>] v:'t array array) = let expr = v |> Array.map(Array.map(fun e -> <@ e @>)) in Matrix<'dim0, 'dim1, 't>(expr)
    new(v: Expr<'t list list>) = let expr = v |> expand_lists' |> List.map(List.toArray) |> List.toArray in Matrix<'dim0, 'dim1, 't>(expr)
    new(d:'t list list) = Matrix<'dim0, 'dim1, 't>((List.map(List.toArray) >> List.toArray) d)
    
    static member create([<ParamArray>] data: 't array array) = Matrix<'dim0, 'dim1,'t>(data)
        
    static member Zero:Matrix<'dim0, 'dim1, 't> = let e = Array.create (number<'dim0>.IntVal) (Array.create (number<'dim1>.IntVal) (zero_val(typeof<'t>) |> expand''<'t>)) in Matrix<'dim0, 'dim1, 't> e

    static member One:Matrix<'dim0, 'dim1, 't> = let e = Array.create (number<'dim1>.IntVal) (Array.create (number<'dim1>.IntVal) (one_val(typeof<'t>) |> expand''<'t>)) in Matrix<'dim0, 'dim1, 't> e

    static member (+) (l: Vector<'dim0, 't>, r: Vector<'dim0, 't>) = 
        let e = defaultLinearAlgebraSymbolicOps.Add l.Expr r.Expr in Vector<'dim0, 't>(e)
    
    static member (-) (l: Vector<'dim0, 't>, r: Vector<'dim0, 't>) = 
        let e = defaultLinearAlgebraSymbolicOps.Subtract l.Expr r.Expr in Vector<'dim0, 't>(e)

    static member (*) (l: Vector<'dim0, 't>, r: Vector<'dim0, 't>) = 
        let e = defaultLinearAlgebraSymbolicOps.InnerProduct l.Expr r.Expr in Scalar<'t> e

    static member (*) (l: Scalar<'t>, r: Vector<'dim0, 't>) = 
        r.Expr |> Array.map(fun e -> expand''<'t> <| call_mul (l.Expr) e) |> Vector<'n, 't>

    static member (*) (l: Vector<'dim0, 't>, r: Scalar<'t>) = 
        l.Expr |> Array.map(fun e -> expand''<'t> <| call_mul e (r.Expr) ) |> Vector<'n, 't>

    static member (*) (l: Vector<'dim0, 't>, r: 't) : Vector<'dim0, 't> = let r' = Scalar<'t>(r) in l * r' 

    static member (*) (l: 't, r: Vector<'dim0, 't>) : Vector<'dim0, 't> = let l' = Scalar<'t>(l) in l' * r

    static member (~-) (l: Vector<'dim0, 't>) =
        l.Expr |> Array.map(call_neg >> expand''<'t>) |> Vector<'n, 't>

type Mat<'dim0, 'dim1 when 'dim0 :> Number and 'dim1:> Number> = Matrix<'dim0, 'dim1, real>
type ComplexMat<'dim0, 'dim1 when 'dim0 :> Number and 'dim1:> Number> = Matrix<'dim0, 'dim1, complex>
type MatQ<'dim0, 'dim1 when 'dim0 :> Number and 'dim1:> Number> = Matrix<'dim0, 'dim1, rat>

module Matrix =
    let madd (l:Vector<'n, 't>) (r:Vector<'n, 't>) = l + r
    
    let msub (l:Vector<'n, 't>) (r:Vector<'n, 't>) = l - r
    
    let msmul (l:'t) (r:Vector<'n, 't>) = Vector<'n, 't>.(*) (l, r)

    let msimplify (l:Vector<_,_>) = l.Expr |> Array.map simplify' |> Vector<_,_>

    //let vnorm (l:Vector<'n, 't>) =
//        let p = l * l in p |> simplify |> call_sqrt |> expand''<'t>  |> Scalar<'t> 

 //   let vdist (l:Vector<'n, 't>) (r:Vector<'n, 't>) = (l - r) |> vnorm |> simplify |> Scalar<'t>