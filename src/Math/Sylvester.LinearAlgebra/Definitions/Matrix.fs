namespace Sylvester

open System

open FSharp.Quotations

open MathNet.Numerics

open Vector

[<StructuredFormatDisplay("{UnicodeDisplay}")>]
type Matrix<'t when 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t>>
    (e: Expr<'t> array array, ?h:TermHistory) = 
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
     
    static member ofCols (data:Expr<'t> [][]) = data |> LinearAlgebraOps.transpose_mat |> Matrix<'t>

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
        [| for i in 0..r.Dim1 - 1 -> l * r.Columns.[i] |] |> Array.map vexpr  |> LinearAlgebraOps.transpose_mat |> Matrix<'t>

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
        [| for i in 0..r.Dims.[1] - 1 -> mvmul l r.Columns.[i] |] |> Array.map vexpr  |> LinearAlgebraOps.transpose_mat |> Matrix<'t>

 



    