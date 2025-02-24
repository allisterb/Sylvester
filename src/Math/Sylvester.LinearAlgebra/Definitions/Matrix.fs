namespace Sylvester

open System

open FSharp.Quotations

open MathNet.Numerics

open Arithmetic
open Dimension
open Vector

[<StructuredFormatDisplay("{UnicodeDisplay}")>]
type Matrix<'t when 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t>>
    internal(e: Expr<'t> array array, ?h:TermHistory) = 
    do if e |> Array.exists (fun a -> a.Length <> e.[0].Length) then failwith "The length of each column in a matrix must be the same."
    let expr = e  |> Array.map (Array.map expand_as<'t>)
    let exprmn = Array.map (Array.map MathNetExpr.fromQuotation) expr
    let exprt = expr |> Array.transpose
    let expr2d = expr |> array2D

    member val Expr = expr
    member val ExprT = exprt
    member val Expr2D = expr2d
    member val ExprMathNet = exprmn
    member val ExprVars = expr |> Array.map (Array.map(get_vars >> List.toArray)) |> Array.concat
    member val Rows = expr |> Array.map Vector<'t>
    member val Columns = exprt |> Array.map Vector<'t>
    member val Dim0 = expr.Length
    member val Dim1 = exprt.Length

    member x.Transpose = exprt |> Matrix<'t> 
    member x.HasSameDims(m:Matrix<'t>) = x.Dim0 = m.Dim0 && x.Dim1 = m.Dim1
    member x.UnicodeDisplay = 
        let replace (o:string) n (s:string) = s.Replace(o, n) 
        x.Rows
        |> Array.skip 1 
        |> Array.fold(fun s e -> sprintf "%s, %s" s (e.UnicodeDisplay)) (x.Rows.[0].UnicodeDisplay) 
        |> replace "(" "["
        |> replace ")" "]"
        |> sprintf "[%s]"
    member x.Item with get i = x.Rows.[i] and set i value = Array.set x.Rows i value
    
    member x.AsNumeric() = 
        let t = typeof<'t>
        match t with
        | LinearAlgebraNumericOpType -> expr |> Array.map (Array.map (evaluate >> Convert.ToDouble)) |> LinearAlgebra.DenseMatrix.ofRowArrays
        | _ -> failwithf "The type %A is not compatible with numeric linear algebra operations." t
    
    interface IEquatable<Matrix<'t>> with
        member a.Equals b = a.UnicodeDisplay = b.UnicodeDisplay

    interface IHtmlDisplay with
           member x.Html() =
               let elems =
                   x.Rows 
                   |> Array.map (fun v -> v.Expr  |> Array.skip 1 |> Array.fold(fun s e -> sprintf "%s & %s" s (latexe e)) (latexe v.Expr.[0])) 
                   |> Array.reduce(fun s e -> sprintf "%s \\\\ %s" s e) 
                   |> sprintf "%s"
               "$$ \\begin{pmatrix} " + elems + " \\end{pmatrix} $$"
    
    interface IMatrix<'t> with
        member val Dims = [| expr2d.GetLength 0; expr2d.GetLength 1 |]
        member val Expr = expr
        member val ExprT = exprt
        member val Expr2D = expr2d
        member val ExprMathNet = exprmn
        member val ExprVars = expr |> Array.map (Array.map(get_vars >> List.toArray)) |> Array.concat |> Array.concat
        member val Rows = expr |> Array.map Vector<'t> 
        member val Columns = exprt |> Array.map Vector<'t>
        member x.Transpose = exprt |> Matrix<'t> 
        member x.Item(i:int, j:int) = expr2d.[i, j] |> Scalar<'t>

    new(d: Expr<'t> [,]) = let d' = d |> Array2D.toJagged in Matrix<'t> d'
    
    new([<ParamArray>] v:'t array array) = let expr = v |> Array.map(Array.map(exprv)) in Matrix<'t>(expr)

    new (s:Scalar<'t> [][]) = let expr = s |> Array.map(Array.map sexpr) in Matrix<'t> expr

    new(rows: Vector<'t> array) = let e = rows |> Array.map(fun a -> a.Expr) in Matrix<'t> e

    static member create([<ParamArray>] data: 't array array) = Matrix<'t>(data)

    static member ofRows (data:Expr<'t> [] []) = Matrix<'t> data
     
    static member ofCols (data:Expr<'t> [][]) = data |> Ops.transpose_mat |> Matrix<'t>

    static member (+) (l: Matrix<'t>, r: Matrix<'t>) = 
        do if not <| l.HasSameDims r then failwith "Matrices for addition must have the same dimension."    
        Array.map2 (+) l.Rows r.Rows |> Matrix<'t> 
        
    static member (-) (l: Matrix<'t>, r: Matrix<'t>) = 
        do if not <| l.HasSameDims r then failwith "Matrices for subtraction must have the same dimension."    
        Array.map2 (-) l.Rows r.Rows |> Matrix<'t> 

    static member (*) (l: Scalar<'t>, r: Matrix<'t>) = 
        r.Rows |> Array.map ((*) l) |> Matrix<'t> 

    static member (*) (l: Matrix<'t>, r: Scalar<'t>) = 
        l.Rows |> Array.map (fun v -> v * r) |> Matrix<'t>

    static member (*) (l:Matrix<'t>, r:Vector<'t>) =
        [| for i in 0..l.Dim0 -> l.Rows.[i] * r |] |> Array.map sexpr |> Vector<'t>

    static member (*) (l:Matrix<'t>, r:Matrix<'t>) =             
        [| for i in 0..r.Dim1 - 1 -> l * r.Columns.[i] |] |> Array.map vexpr  |> Ops.transpose_mat |> Matrix<'t>

    static member (|+|) (l:Matrix<'t>, r:Vector<'t>) = 
        if l.Dim1 <> r.Length then failwithf "The length of the vector (%A) must be the same as the number of columns in the matrix to augment (%A)." r.Length l.Dim1
        Array.append l.Rows [|r|] |> Array.map vexpr |> array2D |> Array2D.transpose |> Array2D.toJagged |> Matrix<'t>

    static member (||+||) (l:Matrix<'t>, r:Vector<'t>) = 
        if l.Dim1 <> r.Length then failwith "The length of the vector must be the same as the number of columns in the matrix to augment."
        Array.append [|r|] l.Rows |> Array.map vexpr |> array2D |> Array2D.transpose |> Array2D.toJagged |> Matrix<'t>

    static member (|+||) (l:Matrix<'t>, r:Vector<'t>) = 
        if l.Dim0 <> r.Length then failwith "The length of the vector must be the same as the number of rows in the matrix to augment."
        Array.append l.Columns [|r|] |> Array.map vexpr |> array2D |> Array2D.transpose |> Array2D.toJagged |> Matrix<'t>

    static member (||+|) (l:Matrix<'t>, r:Vector<'t>) = 
        if l.Dim0 <> r.Length then failwith "The length of the vector must be the same as the number of rows in the matrix to augment."
        Array.append l.Columns [|r|] |> Array.map vexpr |> array2D |> Array2D.transpose |> Array2D.toJagged |> Matrix<'t>
   
and IMatrix<'t when 't: equality and 't :> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t>> = 
    inherit IPartialShape<dim<2>>
    abstract Expr: Expr<'t>[][]
    abstract ExprT: Expr<'t>[][]
    abstract Expr2D: Expr<'t>[,]
    abstract ExprMathNet:MathNetExpr[][]
    abstract ExprVars: Var[]
    abstract Rows: Vector<'t>[]
    abstract Columns: Vector<'t>[]
    abstract Transpose: Matrix<'t>
    abstract Item:int*int->Scalar<'t>
    
type Mat = Matrix<real>
type MatQ = Matrix<rat>
type MatZ = Matrix<int>

[<StructuredFormatDisplay("{UnicodeDisplay}")>]
type Matrix<'dim0, 'dim1, 't when 'dim0 :> Number and 'dim1 :> Number and 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable>
    internal (expr: Expr<'t> array array, ?h:TermHistory) =
    inherit Matrix<'t>(expr,?h=h)
    let dim0 = number<'dim0>
    let dim1 = number<'dim1>
    let exprt = expr |> Ops.transpose_mat
    do if expr.Length <> dim0.IntVal || expr.[0].Length <> dim1.IntVal then failwithf "The initializing array has dimensions [%i][%i] instead of [%i][%i]." expr.Length expr.[0].Length dim0.IntVal dim1.IntVal
    
    member val Dim0:'dim0 = dim0
    member val Dim1:'dim1 = dim1
    member val Rows = expr |> Array.map Vector<'dim1, 't>  
    member val Columns = exprt |> Array.map Vector<'dim0, 't>
    
    member x.Transpose = Matrix<'dim1, 'dim0, 't> exprt
   
    member x.Item(i: int) = x.Rows.[i]
    
    member x.Kr = fun (i:int) (j:int) -> if i = j then x.[i].[j] else expand_as<'t>(zero_val(typeof<'t>)) |> Scalar
    
    interface IEquatable<Matrix<'dim0, 'dim1, 't>> with
        member a.Equals b = a.UnicodeDisplay = b.UnicodeDisplay

    new(d: Expr<'t> [,]) = let d' = d |> Array2D.toJagged in Matrix<'dim0, 'dim1, 't> d'
    
    new([<ParamArray>] v:'t array array) = let expr = v |> Array.map(Array.map(exprv)) in Matrix<'dim0, 'dim1, 't> expr

    new (_:'dim0, _:'dim1, data:Expr<'t> [] []) = Matrix<'dim0, 'dim1, 't> data
    
    new(rows: Vector<'dim1, 't> array) = let e = rows |> Array.map(fun a -> a.Expr) in Matrix<'dim0, 'dim1, 't> e
    
    static member ofRows (data:Expr<'t> [] []) = Matrix<'dim0, 'dim1, 't> data
    
    static member ofCols (data:Expr<'t> [][]) = data |> Ops.transpose_mat |> Matrix<'dim0, 'dim1, 't>

    static member Zero:Matrix<'dim0, 'dim1, 't> = let e = Array.create (number<'dim0>.IntVal) (Array.create (number<'dim1>.IntVal) (zero_val(typeof<'t>) |> expand_as<'t>)) in Matrix<'dim0, 'dim1, 't> e

    static member One:Matrix<'dim1, 'dim1, 't> = Ops.identity_mat number<'dim1>.IntVal |> Matrix<'dim1, 'dim1, 't>

    static member (+) (l: Matrix<'dim0, 'dim1, 't>, r: Matrix<'dim0, 'dim1, 't>) = 
        let m = Array.map2 (+) l.Rows r.Rows in Matrix<'dim0, 'dim1, 't> m
           
    static member (-) (l: Matrix<'dim0, 'dim1, 't>, r: Matrix<'dim0, 'dim1, 't>) = 
        let m = Array.map2 (-) l.Rows r.Rows in Matrix<'dim0, 'dim1, 't> m

    static member (*) (l: Scalar<'t>, r: Matrix<'dim0, 'dim1, 't>) = 
        let m = r.Rows |> Array.map ((*) l) in Matrix<'dim0, 'dim1, 't> m

    static member (*) (l: Matrix<'dim0, 'dim1, 't>, r: Scalar<'t>) = 
         let m = l.Rows |> Array.map (fun v -> v * r) in Matrix<'dim0, 'dim1, 't> m

    static member (*) (l: 't, r: Matrix<'dim0, 'dim1, 't>) = (l |> exprv |> Scalar) * r

    static member (*) (l: int, r: Matrix<'dim0, 'dim1, real>) = (realterm l) * r

    static member (*) (l: rat, r: Matrix<'dim0, 'dim1, real>) = (realterm l) * r

    static member (*) (l: Natural, r: Matrix<'dim0, 'dim1, real>) = (realterm l) * r

    static member (*) (l: Matrix<'dim0, 'dim1, 't>, r: 't) = l * (r |> exprv |> Scalar)
    
    static member (~-) (l: Matrix<'dim0, 'dim1, 't>) = 
        let m = l.Rows |> Array.map (~-) in Matrix<'dim0, 'dim1, 't> m

    static member (*) (l:Matrix<'dim0, 'dim1, 't>, r:Vector<'dim1, 't>) =
        [| for i in 0..l.Dim0.IntVal - 1 -> l.Rows.[i] * r |] |> Array.map sexpr |> Vector<'dim0, 't>

    static member (*) (l:Matrix<'dim0, 'dim1, 't>, r:Matrix<'dim1, 'dim2, 't>) =             
        [| for i in 0..r.Dim1.IntVal - 1 -> l * r.Columns.[i] |] |> Array.map vexpr  |> Ops.transpose_mat |> Matrix<'dim0, 'dim2, 't>

    static member (^^) (l:Matrix<'dim1, 'dim1, 't>, r:int) = 
         let mutable im = l
         for n = 1 to r - 1 do im <- im * l
         im

type SquareMatrix<'dim0, 't when 'dim0 :> Number  and 't : equality and 't: comparison and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable> = Matrix<'dim0, 'dim0, 't>
type Mat<'dim0, 'dim1 when 'dim0 :> Number and 'dim1:> Number> = Matrix<'dim0, 'dim1, real>
type MatQ<'dim0, 'dim1 when 'dim0 :> Number and 'dim1:> Number> = Matrix<'dim0, 'dim1, rat>
type MatZ<'dim0, 'dim1 when 'dim0 :> Number and 'dim1:> Number> = Matrix<'dim0, 'dim1, int>

module Matrix =
    let mat (data:obj list list) = data |>  List.map (List.map realterm >> List.toArray) |> List.toArray  |> Mat

    let madd (l:IMatrix<'t>) (r:IMatrix<'t>) = 
        do if l.Dims.[0] <> r.Dims.[0] || l.Dims.[1] <> r.Dims.[1] then failwithf "Two matrices must have the same dimensions to be conformable for addition."
        Array.map2 (+) l.Rows r.Rows |> Matrix<'t> 

    let msub (l:IMatrix<'t>) (r:IMatrix<'t>) = 
        do if l.Dims.[0] <> r.Dims.[0] || l.Dims.[1] <> r.Dims.[1] then failwithf "Two matrices must have the same dimensions to be conformable for subtraction."
        Array.map2 (-) l.Rows r.Rows |> Matrix<'t> 
    
    let mvmul (l:IMatrix<'t>) (r:IVector<'t>) =
        [| for i in 0..l.Dims.[0] - 1 -> vdot l.Rows.[i]  r |] |> Array.map sexpr |> Vector<'t>
    
    let mmul (l:IMatrix<'t>) (r:IMatrix<'t>) = 
        do if l.Dims.[1] <> r.Dims.[0] then failwith "The two matrices are not conformable for matrix multiplication."
        [| for i in 0..r.Dims.[1] - 1 -> mvmul l r.Columns.[i] |] |> Array.map vexpr  |> Ops.transpose_mat |> Matrix<'t>

 

module MatrixT =
    let (|MatrixR|_|) (m:Matrix<_,_,_>) = m.Rows |> Array.toList |> Some

    let (|MatrixC|_|) (m:Matrix<_,_,_>) = m.Columns |> Array.toList |> Some

    let mexpr (m:Matrix<_,_,_>) = m.Expr

    let mexpri (m:Matrix<_,_,_>) = m.Expr |> Array.indexed

    let mexprt (m:Matrix<_,_,_>) = m.ExprT

    let mexprit (m:Matrix<_,_,_>) = m.ExprT |> Array.indexed

    let mat (l:'dim0) (r:'dim1) (data:obj list) = data |> List.toArray |> realterms |> Array.map sexpr |> Array.chunkBySize (number<'dim1>.IntVal) |> Matrix<'dim0, 'dim1, real>
    
    let sqmat (l:'dim0) (data:obj seq) = data |> realterms |> Array.map sexpr |> Array.chunkBySize (number<'dim0>.IntVal) |> SquareMatrix<'dim0, real>

    let sqmat2 (data:obj seq) = data |> realterms |> Array.map sexpr |> Array.chunkBySize 2 |> SquareMatrix<``2``, real>

    let sqmat3 (data:obj seq) = data |> realterms |> Array.map sexpr |> Array.chunkBySize 3 |> SquareMatrix<``3``, real>

    let sqmat4 (data:obj seq) = data |> realterms |> Array.map sexpr |> Array.chunkBySize 4 |> SquareMatrix<``4``, real>

    let sqmat5 (data:obj seq) = data |> realterms |> Array.map sexpr |> Array.chunkBySize 5 |> SquareMatrix<``5``, real>

    let sqmat6 (data:obj seq) = data |> realterms |> Array.map sexpr |> Array.chunkBySize 6 |> SquareMatrix<``6``, real>

    let mata (l:'dim0) (r:'dim1) (data:Expr<'t>[] []) = Matrix<'dim0, 'dim1, 't> data
    
    let matv (v:Expr<'t>) = 
        let e = Array.create (number<'dim0>.IntVal) (Array.create (number<'dim1>.IntVal) v) in 
        Matrix<'dim0, 'dim1, 't> e

    let mata_cols (l:'dim0) (r:'dim1) (data:Expr<'t>[] []) = Matrix<'dim0, 'dim1, 't>.ofCols data

    let inline (|+||) (l:Matrix<'dim0, 'dim1, 't>) (r:Vector<'dim0, 't>) = 
        let data = Array.append l.Columns [|r|] |> Array.map vexpr |> array2D |> Array2D.transpose |> Array2D.toJagged 
        Matrix<_,_,'t> (l.Dim0, (pp (l.Dim1 + ``1``)), data)

    let inline (||+|) (l:Vector<'dim0, 't>) (r:Matrix<'dim0, 'dim1, 't>)  = 
        let data = Array.append [|l|] r.Columns |> Array.map vexpr |> array2D |> Array2D.transpose |> Array2D.toJagged 
        Matrix<_,_,'t> (l.Dim0, (pp (r.Dim1 + ``1``)), data)

    let inline (||+||) (l:Matrix<'dim0, 'dim1, 't>) (r:Vector<'dim1, 't>) = 
        Array.append l.Rows [|r|] |> Array.map vexpr |> array2D |> Array2D.toJagged |> mata_cols (pp (l.Dim0 + ``1``)) l.Dim1 

    let mzero (l:'dim0) (r:'dim1)= Matrix<'dim0, 'dim1, real>.Zero

    let mident (l:'dim0) = Matrix<'dim0, 'dim0, real>.One

    let mtrans (m:Matrix<'dim0, 'dim1, 't>) = m.Transpose

    let madd (l:Matrix<'dim0, 'dim1, 't>) (r:Matrix<'dim0, 'dim1, 't>) = l + r
    
    let msub (l:Matrix<'dim0, 'dim1, 't>) (r:Matrix<'dim0, 'dim1, 't>) = l - r
    
    let msmul (l:'t) (r:Matrix<'dim0, 'dim1, 't>) = Matrix<'dim0, 'dim1, 't>.(*) (l, r)

    let msimplify (l:Matrix<'dim0, 'dim1, 't>) = l.Expr |> Array.map (Array.map simplifye) |> Matrix<'dim0, 'dim1, 't>

    let inline mrmul i (k:Scalar<'t>) (l:Matrix<'dim0, 'dim1, 't>)=
        if i >= l.Dim0.IntVal then failwith "row index"
        let rows = l.Rows.Clone() :?> Vector<'dim1, 't> array
        let ri = k * l.[i]
        rows.[i] <- ri
        Matrix<'dim0, 'dim1, 't> rows

    let inline mrswitch i j (l:Matrix<'dim0, 'dim1, 't>) =
        if i >= l.Dim0.IntVal then failwith "row index"
        if j >= l.Dim1.IntVal then failwith "row index"
        let ri = l.[i] 
        let rj = l.[j]
        let rows = l.Rows.Clone() :?> Vector<'dim1, 't> array
        rows.[i] <- rj
        rows.[j] <- ri
        Matrix<'dim0, 'dim1, 't> rows
    
    let inline mraddmul i j (k:Scalar<'t>) (l:Matrix<'dim0, 'dim1, 't>) =
        if i >= l.Dim0.IntVal then failwith "row index"
        if j >= l.Dim1.IntVal then failwith "row index"

        let rows = l.Rows.Clone() :?> Vector<'dim1, 't> array
        let ri = l.[i] + k * l.[j] 
        rows.[i] <- ri
        Matrix<'dim0, 'dim1, 't> rows

    let inline mdiag (l:Matrix<'dim0,'dim1,'t>) = 
        let dim = Math.Min(l.Dim0.IntVal, l.Dim1.IntVal)
        Array2D.init dim dim l.Kr
        |> Array2D.toJagged
        |> sexprs2
        |> mata_cols (min l.Dim0 l.Dim1) (min l.Dim0 l.Dim1)

    let inline mdelr n (m:Matrix<_,_,_>) =
        check (n +< m.Dim0)
        m |> mexpri |> Array.filter(fun (i, r) -> i <> (int) n) |> Array.map snd |> mata (pp (m.Dim0 - ``1``)) m.Dim1
    
    let inline mdelc n (m:Matrix<_,_,_>) =
        check (n +< m.Dim1)
        m |> mexprit |> Array.filter(fun (i, _) -> i <> (int) n) |> Array.map snd |> mata_cols m.Dim0 (pp (m.Dim1 - ``1``))
    
    let det (l:SquareMatrix<'dim0, _>) =
        match l.Dim0.IntVal with
        | 1 -> l.[0].[0]
        | 2 -> l.[0].[0] * l.[1].[1] - l.[0].[1] * l.[1].[0]
        | _ -> failwith "Not supported" 

    let zero<'dim0,'dim1,'t when 'dim0 :> Number and 'dim1 :> Number and 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable> = Matrix<'dim0, 'dim1, 't>.Zero

    let identity<'dim0,'dim1,'t when 'dim0 :> Number and 'dim1 :> Number and 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable> = Matrix<'dim0, 'dim1, 't>.One

[<AutoOpen>]
module Matrices =
    
    open Matrix

    //let mat21 e0 e1 e2 = mat ``2`` ``1`` [e0; e1; e2] 


    