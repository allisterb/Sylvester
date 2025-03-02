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
    member x.Item with get (i,j) = expr2d.[i,j] |> Scalar<'t>

    member x.AsNumeric() = 
        let t = typeof<'t>
        match t with
        | LinearAlgebraNumericOpType -> expr |> Array.map (Array.map (evaluate >> Convert.ToDouble)) |> LinearAlgebra.DenseMatrix.ofRowArrays
        | _ -> failwithf "The type %A is not compatible with numeric linear algebra operations." t
    
     member x.Kr = fun (i:int) (j:int) -> if i = j then x.[i].[j] else expand_as<'t>(zero_val(typeof<'t>)) |> Scalar

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

    static member ofRows (data:Scalar<'t> [] []) = Matrix<'t> data
    
    static member ofCols (data:Scalar<'t> [][]) = data |> sexprs2 |> Matrix<'t>.ofCols

    static member ofRows (data:Vector<'t> []) = Matrix<'t> data
       
    static member ofCols (data:Vector<'t> []) = (Matrix<'t> data).Transpose

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
        [| for i in 0..l.Dim0 - 1 -> l.Rows.[i] * r |] |> Array.map sexpr |> Vector<'t>

    static member (*) (l:Matrix<'t>, r:Matrix<'t>) =             
         [| for i in 0..r.Dim1 - 1 -> l * r.Columns.[i] |] |> Array.map vexpr  |> LinearAlgebraOps.transpose_mat |> Matrix<'t>

    static member (/) (l: Matrix<'t>, r: Scalar<'t>) = 
           l.Rows |> Array.map (vsdiv r) |> Matrix<'t> 

    static member (|+|) (l:Matrix<'t>, r:Vector<'t>) = 
        if l.Dim1 <> r.Length then failwith "The length of the vector must be the same as the number of columns in the matrix to augment."
        Array.append [|r|] l.Rows |> Array.map vexpr |> array2D |> Array2D.transpose |> Matrix<'t>
    
    static member (||+||) (l:Matrix<'t>, r:Vector<'t>) = 
        if l.Dim1 <> r.Length then failwithf "The length of the vector (%A) must be the same as the number of columns in the matrix to augment (%A)." r.Length l.Dim1
        Array.append l.Rows [|r|] |> Array.map vexpr |> array2D |> Array2D.transpose |> Matrix<'t>

    static member (|+||) (l:Matrix<'t>, r:Vector<'t>) = 
        if l.Dim0 <> r.Length then failwith "The length of the vector must be the same as the number of rows in the matrix to augment."
        Array.append l.Columns [|r|] |> Array.map vexpr |> array2D |> Array2D.transpose |> Matrix<'t>

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

type BlockMatrix<'t when 't: equality and 't :> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t>>(rowgroup:int seq,colgroup:int seq, m:IMatrix<'t>) =
    do 
        if m.Dims.[0] <>  m.Dims.[1] then failwithf "This matrix is not square and has dimensions %Ax%A" m.Dims.[0] m.Dims.[1]
        if rowgroup |> Seq.sum <> m.Dims.[0] then failwith "The size of the row groups collection is not the same as the number of rows in the matrix"
        if colgroup |> Seq.sum <> m.Dims.[1] then failwith "The size of the column groups collection is not the same as the number of rows in the matrix"

    let mblock i0 j0 i1 j1 (m:IMatrix<_>) = [|for i in i0 .. i1 -> [| for j in j0 .. j1 -> m.[i, j] |] |] |> Matrix<_> in  
    let prepend (a:int) (arr:int[]) :int[] = Seq.concat[ seq{a}; arr :> seq<int>] |> Seq.toArray in
    let rowindices = rowgroup |> Seq.toArray |> Array.mapFold (fun i j -> i + j , (i+j)) 0 |> fst |> prepend 0 |> Array.windowed 2
    let colindices = colgroup |> Seq.toArray |> Array.mapFold (fun i j -> i + j , (i+j)) 0 |> fst |> prepend 0 |> Array.windowed 2        
    let blocks = [|for i in 0..rowindices.Length - 1 -> 
                    [| for j in 0..colindices.Length - 1 -> 
                        mblock (rowindices.[i].[0]) (colindices.[j].[0]) (rowindices.[i].[1] - 1) ( colindices.[j].[1] - 1) m |] |]
    let blocksT = Array.transpose blocks
    let expr = m.Expr
    let exprt = m.ExprT
    let expr2d = m.Expr2D
    
    member val Parent = m
    member val Expr = expr
    member val ExprT = exprt
    member val Expr2D = expr2d
    member val Rows = blocks
    member val Columns = blocksT
    member val Dim0 = blocks.Length
    member val Dim1 = blocksT.Length

module Matrix =

    let fail_if_invalid_row_index i (m:IMatrix<'t>)  = 
        let rcount = m.Rows.Length
        if i >= rcount then failwithf "The row index %A is greater than the number of rows in the matrix: %A." i rcount 

    let fail_if_invalid_row_indices (i:seq<int>) (m:IMatrix<'t>) = let f n = fail_if_invalid_row_index n m in i |> Seq.iter f 

    let fail_if_invalid_col_index j (m:IMatrix<'t>) = 
           let ccount = m.Columns.Length
           if j >= ccount then failwithf "The column index %A is greater than the number of columns in the matrix: %A." j ccount 

    let fail_if_invalid_indices i j (m:IMatrix<'t>) = 
        fail_if_invalid_row_index i m
        fail_if_invalid_col_index j m
    
    let fail_if_not_square (m:IMatrix<_>) = if m.Dims.[0] <>  m.Dims.[1] then failwithf "This matrix is not square and has dimensions %Ax%A" m.Dims.[0] m.Dims.[1]
    
    let mat (data:obj seq seq) = data |>  Seq.map (Seq.map realterm >> Seq.toArray) |> Seq.toArray  |> Mat

    let sqmat (data: obj seq) =
        let n = Seq.length data
        do if n |> is_perfect_square |> not then failwithf "The number of matrix elements must be a perfect square."
        data |> Seq.map realterm |> Seq.splitInto (n |> float |> sqrt |> int) |> Seq.toArray |> Mat

    let matc (data:obj seq seq) =  data |>  Seq.map (Seq.map (realterm >> sexpr) >> Seq.toArray) |> Seq.toArray |> Matrix<real>.ofCols 

    let expr (m:IMatrix<_>) = m.Expr
    
    let expri (m:IMatrix<_>) = m.Expr |> Array.indexed
    
    let exprt (m:IMatrix<_>) = m.ExprT
    
    let exprit (m:IMatrix<_>) = m.ExprT |> Array.indexed

    let expr2d (m:IMatrix<_>) = m.Expr2D
    
    let melems (m:IMatrix<_>) = m |> expr |> Array.map(Array.map Scalar<_>)

    let melems2d (m:IMatrix<_>) = m |> expr2d |> Array2D.map Scalar<_>

    let mmap map (m:IMatrix<_>) = [| for i in 0..m.Dims.[0] - 1 -> [| for j in 0..m.Dims.[1] - 1 -> map i j m |] |]
    
    let trans (m:IMatrix<_>) = m.Transpose

    let madd (l:IMatrix<'t>) (r:IMatrix<'t>) = 
        do if l.Dims.[0] <> r.Dims.[0] || l.Dims.[1] <> r.Dims.[1] then failwithf "Two matrices must have the same dimensions to be conformable for addition."
        Array.map2 (+) l.Rows r.Rows |> Matrix<'t> 

    let msub (l:IMatrix<'t>) (r:IMatrix<'t>) = 
        do if l.Dims.[0] <> r.Dims.[0] || l.Dims.[1] <> r.Dims.[1] then failwithf "Two matrices must have the same dimensions to be conformable for subtraction."
        Array.map2 (-) l.Rows r.Rows |> Matrix<'t> 
    
    let msmul (l:Scalar<'t>) (r:IMatrix<'t>) = r.Rows |> Array.map ((*) l) |> Matrix<'t> 
          
    let msdiv (l:Scalar<'t>) (r:IMatrix<'t>) = r.Rows |> Array.map (vsdiv l) |> Matrix<'t>
    
    let mvmul (l:IMatrix<'t>) (r:IVector<'t>) =
        [| for i in 0..l.Dims.[0] - 1 -> vdot l.Rows.[i]  r |] |> Array.map sexpr |> Vector<'t>
    
    let mmul (l:IMatrix<'t>) (r:IMatrix<'t>) = 
        do if l.Dims.[1] <> r.Dims.[0] then failwith "The two matrices are not conformable for matrix multiplication."
        [| for i in 0..r.Dims.[1] - 1 -> mvmul l r.Columns.[i] |] |> Array.map vexpr  |> LinearAlgebraOps.transpose_mat |> Matrix<'t>

    let mrmul i (k:Scalar<'t>) (l:Matrix<'t>)=
        do fail_if_invalid_row_index i l
        let rows = l.Rows.Clone() :?> Vector<'t> array
        let ri = k * l.[i]
        rows.[i] <- ri
        Matrix<'t> rows

    let rswitch i j (l:Matrix<'t>) =
        do fail_if_invalid_row_indices [i;j] l
        let ri = l.[i] 
        let rj = l.[j]
        let rows = l.Rows.Clone() :?> Vector<'t> array
        rows.[i] <- rj
        rows.[j] <- ri
        Matrix<'t> rows

    let raddmul i j (k:Scalar<'t>) (l:Matrix<'t>) =
        do fail_if_invalid_row_indices [i;j] l
        let rows = l.Rows.Clone() :?> Vector<'t> array
        let ri = l.[i] + k * l.[j] 
        rows.[i] <- ri
        Matrix<'t> rows

    let crep j (v:Vector<_>) (l:Matrix<_>) =
        do 
            fail_if_invalid_col_index j l
            if v.Length <> l.Dim0 then failwithf "The length of the column vector (%A) is not the same as then number of rows (%A)" v.Length l.Dim0
        let cols = l.Columns.Clone() :?> Vector<'t> array
        cols.[j] <- v
        Matrix<'t>.ofCols cols
        
    let diag (l:Matrix<'t>) = 
        let dim = Math.Min(l.Dim0, l.Dim1)
        Array2D.init dim dim l.Kr
        |> Array2D.toJagged
        |> sexprs2
        |> Matrix<'t>.ofCols

    let rdel n (m:IMatrix<_>) =
        do fail_if_invalid_row_index n m
        m |> expri |> Array.filter(fun (i, _) -> i <> n) |> Array.map snd |> Matrix<'t>.ofRows
    
    let cdel n (m:IMatrix<_>) =
        do fail_if_invalid_col_index n m
        m |> exprit |> Array.filter(fun (i, _) -> i <> (int) n) |> Array.map snd |> Matrix<'t>.ofCols
    
    let submat i j (m:IMatrix<_>) = m |> rdel i |> cdel j

    let rec det (m:IMatrix<_>) =
        do fail_if_not_square m
        let n = m.Dims.[0] in
        match n with
        | 1 -> m.[0,0]
        | 2 -> m.[0,0] * m.[1,1] - m.[0,1] * m.[1, 0]
        | _ -> [| for i in 0..n - 1 -> m |> submat 0 i |> det |> (*) ((s_neg_one *** i) * m.[0, i]) |] |> Array.reduce (+) 

    let minor i j (m:IMatrix<_>) = m |> submat i j |> det

    let cofactor i j (m:IMatrix<_>) = m |> minor i j |> (*) (s_neg_one *** (i+j))

    let r_coexpand i (m:IMatrix<_>) =
        [|for j in 0 .. m.Dims.[1] - 1 -> cofactor i j m |] |> Array.reduce (+)

    let c_coexpand j (m:IMatrix<_>) =
        [|for i in 0 .. m.Dims.[0] - 1 -> cofactor i j m |] |> Array.reduce (+)

    let comat m =  m |> mmap cofactor |> Matrix<_>

    let adjoint (m:IMatrix<_>) = m |> comat |> trans
        
    let inverse (m:IMatrix<_>) = adjoint m / det m

    let mblock i0 j0 i1 j1 (m:IMatrix<_>) = [|for i in i0 .. i1 -> [| for j in j0 .. j1 -> m.[i, j] |] |] |> Matrix<_>

    let blockmat (rowgroup:int seq) (colgroup: int seq) (m:IMatrix<_>) = BlockMatrix<_>(rowgroup, colgroup, m)


