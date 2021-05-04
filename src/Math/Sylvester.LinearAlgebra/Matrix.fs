namespace Sylvester

open System
open FSharp.Quotations

open MathNet.Numerics
open Sylvester.Arithmetic
open Dimension
open Vector

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
        fetch_column [] (matrix |> (Array.map(Array.toList) >> Array.toList)) |> (List.map(List.toArray) >> List.toArray)
    let exprT = expr  |> transpose 
    member val Expr = expr
    member val ExprVars = expr |> Array.map (Array.map(get_vars >>List.toArray)) |> Array.concat
    member val Expr' = expr'
    member val ExprT = exprT
    member val RowVectors = expr |> Array.map Vector<'t>
    member val ColumnVectors = exprT |> Array.map Vector<'t>
    member x.Display = 
        let nl = System.Environment.NewLine
        x.RowVectors
        |> Array.skip 1 
        |> Array.fold(fun s e -> sprintf "%s%s%s" s (nl) (e.LinearDisplay)) (nl + x.RowVectors.[0].LinearDisplay) 
        |> sprintf "%s"
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
    member x.Transpose = Matrix<'dim1, 'dim0, 't>(x.ExprT)
    member x.Item(i: int) = x.RowVectors.[i]
    interface IMatrix<'dim0, 'dim1> with 
        member val Dim0 = dim0
        member val Dim1 = dim1
    new([<ParamArray>] v:'t array array) = let expr = v |> Array.map(Array.map(fun e -> <@ e @>)) in Matrix<'dim0, 'dim1, 't>(expr)
    new(v: Expr<'t list list>) = let expr = v |> expand_lists' |> List.map(List.toArray) |> List.toArray in Matrix<'dim0, 'dim1, 't>(expr)
    new(d:'t list list) = Matrix<'dim0, 'dim1, 't>((List.map(List.toArray) >> List.toArray) d)
    new(rows: Vector<'dim1, 't> array) = let e = rows |> Array.map(fun a -> a.Expr) in Matrix<'dim0, 'dim1, 't>(e)
    new (v:Expr<'t>) = 
        let e = Array.create (number<'dim0>.IntVal) (Array.create (number<'dim1>.IntVal) v) in 
            Matrix<'dim0, 'dim1, 't> e
    static member create([<ParamArray>] data: 't array array) = Matrix<'dim0, 'dim1,'t>(data)
        
    static member Zero:Matrix<'dim0, 'dim1, 't> = let e = Array.create (number<'dim0>.IntVal) (Array.create (number<'dim1>.IntVal) (zero_val(typeof<'t>) |> expand''<'t>)) in Matrix<'dim0, 'dim1, 't> e

    static member One:Matrix<'dim0, 'dim1, 't> = let e = Array.create (number<'dim0>.IntVal) (Array.create (number<'dim1>.IntVal) (one_val(typeof<'t>) |> expand''<'t>)) in Matrix<'dim0, 'dim1, 't> e

    static member (+) (l: Matrix<'dim0, 'dim1, 't>, r: Matrix<'dim0, 'dim1, 't>) = 
        let m = Array.map2 (+) l.RowVectors r.RowVectors in Matrix<'dim0, 'dim1, 't> m
           
    static member (-) (l: Matrix<'dim0, 'dim1, 't>, r: Matrix<'dim0, 'dim1, 't>) = 
        let m = Array.map2 (-) l.RowVectors r.RowVectors in Matrix<'dim0, 'dim1, 't> m

    static member (*) (l: Scalar<'t>, r: Matrix<'dim0, 'dim1, 't>) = 
        let m = r.RowVectors |> Array.map ((*) l) in Matrix<'dim0, 'dim1, 't> m

    static member (*) (l: Matrix<'dim0, 'dim1, 't>, r: Scalar<'t>) = 
         let m = l.RowVectors |> Array.map (fun v -> v * r) in Matrix<'dim0, 'dim1, 't> m

    static member (*) (l: 't, r: Matrix<'dim0, 'dim1, 't>) = scalar l * r

    static member (*) (l: Matrix<'dim0, 'dim1, 't>, r: 't) = l * scalar r
    
    static member (~-) (l: Matrix<'dim0, 'dim1, 't>) = 
        let m = l.RowVectors |> Array.map (~-) in Matrix<'dim0, 'dim1, 't> m

    static member (*) (l:Matrix<'dim0, 'dim1, 't>, r:Vector<'dim1, 't>) =
        seq {for i in 0..l.Dim0.IntVal - 1 -> l.RowVectors.[i] * r} |> Seq.map sexpr |> Seq.toArray |> Vector<'dim0, 't>

    static member (*) (l:Matrix<'dim0, 'dim1, 't>, r:Matrix<'dim1, 'dim2, 't> ) =       
        (* Transposes rectangular matrices *)
        let transpose matrix =
            let rec fetch_column acc (matr:(Expr<'t> list list)) = (* Makes a column list from a row list *)
                if matr.Head.Length = 0 then (List.rev acc) (* Stop *)
                else fetch_column
                        ([for row in matr -> row.Head]::acc) (* Fetches the first item from each row *)
                        (List.map (fun row -> match row with [] -> [] | h::t -> t) matr)
            fetch_column [] (matrix |> (Array.map(Array.toList) >> Array.toList)) |> (List.map(List.toArray) >> List.toArray)
            
        seq {for i in 0..r.Dim1.IntVal - 1 -> l * r.ColumnVectors.[i]} |> Seq.map(vexpr)  |> Seq.toArray |> transpose |> Matrix<'dim0, 'dim2, 't>

type Mat<'dim0, 'dim1 when 'dim0 :> Number and 'dim1:> Number> = Matrix<'dim0, 'dim1, real>
type ComplexMat<'dim0, 'dim1 when 'dim0 :> Number and 'dim1:> Number> = Matrix<'dim0, 'dim1, complex>
type MatQ<'dim0, 'dim1 when 'dim0 :> Number and 'dim1:> Number> = Matrix<'dim0, 'dim1, rat>

module Matrix =
    let mtrans (m:Matrix<'dim0, 'dim1, 't>) = m.Transpose

    let madd (l:Matrix<'dim0, 'dim1, 't>) (r:Matrix<'dim0, 'dim1, 't>) = l + r
    
    let msub (l:Matrix<'dim0, 'dim1, 't>) (r:Matrix<'dim0, 'dim1, 't>) = l - r
    
    let msmul (l:'t) (r:Matrix<'dim0, 'dim1, 't>) = Matrix<'dim0, 'dim1, 't>.(*) (l, r)

    let msimplify (l:Matrix<'dim0, 'dim1, 't>) = l.Expr |> Array.map (Array.map simplify') |> Matrix<'dim0, 'dim1, 't>