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
        member a.Equals b = 
            let ae = a.Expr2D |> Array2D.map simplifye 
            let be = b.Expr2D |> Array2D.map simplifye
            ae.Length = be.Length && ae |> Array2D.forall(fun i j e -> sequal e be.[i,j])

     override a.Equals b = 
        if b :? IMatrix<'t> then
            let ae = a.Expr2D |> Array2D.map simplifye 
            let be = (b :?> IMatrix<'t>) .Expr2D |> Array2D.map simplifye
            ae.Length = be.Length && ae |> Array2D.forall(fun i j e -> sequal e be.[i,j]) else false

     override a.GetHashCode() = let e = a.Expr2D |> Array2D.map simplifye |> Array2D.flatten |> Array.map(sprintf "%A") |> Array.reduce(sprintf "%s,%s")  in e.GetHashCode()
     
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
        member a.Equals b = 
            let ae = a.Expr2D |> Array2D.map simplifye 
            let be = b.Expr2D |> Array2D.map simplifye
            ae.Length = be.Length && ae |> Array2D.forall(fun i j e -> sequal ae.[i,j] be.[i,j])

    new(d: Expr<'t> [,]) = let d' = d |> Array2D.toJagged in Matrix<'t> d'
    
    new([<ParamArray>] v:'t array array) = let expr = v |> Array.map(Array.map(exprv)) in Matrix<'t>(expr)

    new (s:Scalar<'t> [][]) = let expr = s |> Array.map(Array.map sexpr) in Matrix<'t> expr

    new (s:Scalar<'t> [,]) = let a = s |> Array2D.toJagged in Matrix<'t> a
    
    new(rows: Vector<'t> array) = let e = rows |> Array.map(fun a -> a.Expr) in Matrix<'t> e

    static member create([<ParamArray>] data: 't array array) = Matrix<'t>(data)

    static member ofRows (data:Expr<'t> [] []) = Matrix<'t> data
     
    static member ofCols (data:Expr<'t> [][]) = data |> LinearAlgebraOps.transpose_mat |> Matrix<'t>

    static member ofRows (data:Scalar<'t> [] []) = Matrix<'t> data
    
    static member ofCols (data:Scalar<'t> [][]) = data |> sexprs2 |> Matrix<'t>.ofCols

    static member ofCols (data:Scalar<'t> [,]) = data |> Array2D.toJagged |> Matrix<'t>.ofCols

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
        if l.Dim1 <> r.Dim then failwith "The dimension of the vector must be the same as the number of columns in the matrix to augment."
        Array.append [|r|] l.Rows |> Array.map vexpr |> array2D |> Array2D.transpose |> Matrix<'t>
    
    static member (||+||) (l:Matrix<'t>, r:Vector<'t>) = 
        if l.Dim1 <> r.Dim then failwithf "The dimension of the vector (%A) must be the same as the number of columns in the matrix to augment (%A)." r.Dim l.Dim1
        Array.append l.Rows [|r|] |> Array.map vexpr |> array2D |> Array2D.transpose |> Matrix<'t>

    static member (|+||) (l:Matrix<'t>, r:Vector<'t>) = 
        if l.Dim0 <> r.Dim then failwith "The dimension of the vector must be the same as the number of rows in the matrix to augment."
        Array.append l.Columns [|r|] |> Array.map vexpr |> array2D |> Array2D.transpose |> Matrix<'t>

    static member (||+|) (l:Matrix<'t>, r:Vector<'t>) = 
        if l.Dim0 <> r.Dim then failwith "The dimension of the vector must be the same as the number of rows in the matrix to augment."
        Array.append l.Columns [|r|] |> Array.map vexpr |> array2D |> Array2D.transpose |> Array2D.toJagged |> Matrix<'t>
   
    static member (|+||) (l: Matrix<'t>, r: Matrix<'t>) = 
          do if l.Dim0 <> r.Dim0 then failwith "Matrices for column-wise join must have the same number of rows."    
          Array.append l.Columns r.Columns |> Matrix<'t>.ofCols
     
    static member (||+|) (l: Matrix<'t>, r: Matrix<'t>) = 
          do if l.Dim0 <> r.Dim0 then failwith "Matrices for column-wise join must have the same number of rows."    
          Array.append r.Columns l.Columns |> Matrix<'t>.ofCols

    static member (|+|) (l: Matrix<'t>, r: Matrix<'t>) = 
        do if l.Dim1 <> r.Dim1 then failwith "Matrices for row-wise join must have the same number of columns."    
        Array.append r.Rows l.Rows |> Matrix<'t>.ofRows

    static member (||+||) (l: Matrix<'t>, r: Matrix<'t>) = 
           do if l.Dim1 <> r.Dim1 then failwith "Matrices for column-wise join must have the same number of columns."    
           Array.append l.Rows r.Rows |> Matrix<'t>.ofRows

and IMatrix<'t when 't: equality and 't :> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t>> = 
    inherit IPartialShape<dim<2>>
    inherit IEquatable<IMatrix<'t>>
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
    inherit Matrix<'t>(m.Expr)
    do 
        if m.Dims.[0] <>  m.Dims.[1] then failwithf "This matrix is not square and has dimensions %Ax%A" m.Dims.[0] m.Dims.[1]
        if rowgroup |> Seq.sum <> m.Dims.[0] then failwith "The size of the row groups collection is not the same as the number of rows in the matrix"
        if colgroup |> Seq.sum <> m.Dims.[1] then failwith "The size of the column groups collection is not the same as the number of rows in the matrix"

    let mblock i0 j0 i1 j1 (m:IMatrix<_>) = [|for i in i0 .. i1 -> [| for j in j0 .. j1 -> m.[i, j] |] |] |> Matrix<_> 
    let prepend (a:int) (arr:int[]) :int[] = Seq.concat[ seq{a}; arr :> seq<int>] |> Seq.toArray 
    let rowindices = rowgroup |> Seq.toArray |> Array.mapFold (fun i j -> i + j , (i+j)) 0 |> fst |> prepend 0 |> Array.windowed 2
    let colindices = colgroup |> Seq.toArray |> Array.mapFold (fun i j -> i + j , (i+j)) 0 |> fst |> prepend 0 |> Array.windowed 2        
    let blocks = [| for i in 0..rowindices.Length - 1 -> 
                    [| for j in 0..colindices.Length - 1 -> 
                        mblock (rowindices.[i].[0]) (colindices.[j].[0]) (rowindices.[i].[1] - 1) ( colindices.[j].[1] - 1) m 
                    |] 
                 |] |> array2D
    member val Parent = m  
    member val Blocks = blocks

    new (blocks:IMatrix<'t> [][]) =
        let append_cols (l:IMatrix<'t>) (r:IMatrix<'t>) = Array.append l.Columns r.Columns |> Matrix<'t>.ofCols :> IMatrix<'t>
        let append_rows (l:IMatrix<'t>) (r:IMatrix<'t>) = Array.append l.Rows r.Rows |> Matrix<'t> :> IMatrix<'t>
        let mdim0 (m:IMatrix<'t>) = m.Dims.[0] in
        let mdim1 (m:IMatrix<'t>) = m.Dims.[1] in
        let b = blocks |> Array.map (Array.reduce (append_cols)) |> Array.reduce (append_rows) 
        let cg = blocks |> Array.item 0 |> Array.map mdim1
        let rg = blocks |> Array.transpose |> Array.item 0 |> Array.map mdim0
        BlockMatrix<'t>(rg, cg, b)
        
type JordanMatrix<'t when 't: equality and 't :> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t>>(blocks:IMatrix<'t>[][]) =
    inherit BlockMatrix<'t>(blocks)
    
    let is_zero (m:IMatrix<_>) = m.Expr2D |> Array2D.map Scalar<_> |> Array2D.forall(fun _ _ e -> e = zero) 

    let is_jordan_block (m:IMatrix<_>) =
        let l = m.[0,0] in
        let elem2d = m.Expr2D |> Array2D.map Scalar<_> in
        elem2d |> Array2D.forall(fun i j e -> if i = j then e = l else if j = i + 1 then e = one else e = zero)
   
    do if blocks |> array2D |> Array2D.forall(fun i j b -> if i = j then is_jordan_block b else is_zero b) |> not then 
        failwithf "The matrix %A is not a Jordan block." (blocks |> array2D |> Array2D.flatten |> Array.find (not << is_jordan_block)) 

    member val JordanBlocks = [| for i in 0 .. blocks.Length - 1 -> blocks.[i].[i] |]

    new(blocks:IMatrix<'t>[]) =
    
        let is_jordan_block (m:IMatrix<_>) =
            let l = m.[0,0] in
            let elem2d = m.Expr2D |> Array2D.map Scalar<_> in
            m.Dims.[0] = m.Dims.[1] && elem2d |> Array2D.forall(fun i j e -> if i = j then e = l else if j = i + 1 then e = one else e = zero)
                                   
        let zeromat i j = Array2D.init i j (fun _ _ -> zero<'t>) |> Matrix<'t>
   
        let mdim0 (m:IMatrix<'t>) = m.Dims.[0] in
   
        let mdim1 (m:IMatrix<'t>) = m.Dims.[1] in
                  
        do if blocks |> Array.forall is_jordan_block |> not then failwithf "The matrix %A is not a Jordan block." (Array.find(not << is_jordan_block) blocks) 
        let c = blocks.Length
        let m = Array.sumBy mdim0 blocks
        let n = Array.sumBy mdim1 blocks
        do if m <> n then failwith "The supplied blocks do not form a square matrix when joined." 
        let b = [|for i in 0..c - 1 -> [| for j in 0 .. c - 1 -> if i = j then blocks.[i] else zeromat (mdim0 blocks.[i]) (mdim1 blocks.[j]) :> IMatrix<'t>|]|] in
        JordanMatrix<'t> b
        
module Matrix =

    let fail_if_invalid_row_index i (m:IMatrix<'t>)  = 
        let rcount = m.Rows.Length in
        if i >= rcount then failwithf "The row index %A exceeds the number of rows in the matrix: %A." i rcount 

    let fail_if_invalid_row_indices (i:seq<int>) (m:IMatrix<'t>) = let f n = fail_if_invalid_row_index n m in i |> Seq.iter f 

    let fail_if_invalid_col_index j (m:IMatrix<'t>) = 
           let ccount = m.Columns.Length in
           if j >= ccount then failwithf "The column index %A exceeds the number of columns in the matrix: %A." j ccount 

    let fail_if_invalid_col_indices (j:seq<int>) (m:IMatrix<'t>) = let f n = fail_if_invalid_col_index n m in j |> Seq.iter f 

    let fail_if_invalid_indices i j (m:IMatrix<'t>) = 
        fail_if_invalid_row_index i m
        fail_if_invalid_col_index j m
    
    let fail_if_not_square (m:IMatrix<_>) = if m.Dims.[0] <>  m.Dims.[1] then failwithf "This matrix is not square and has dimensions %Ax%A" m.Dims.[0] m.Dims.[1]
    
    let mat (data:obj seq seq) = data |> Seq.map (Seq.map realterm >> Seq.toArray) |> Seq.toArray  |> Mat

    let sqmat (data: obj seq) =
        let n = Seq.length data
        do if n |> is_perfect_square |> not then failwithf "The number of matrix elements in a square matrix must be a perfect square."
        data |> Seq.map realterm |> Seq.splitInto (n |> float |> sqrt |> int) |> Seq.toArray |> Mat

    let matc (data:obj seq seq) =  data |>  Seq.map (Seq.map (realterm >> sexpr) >> Seq.toArray) |> Seq.toArray |> Matrix<real>.ofCols 

    let mexpr (m:IMatrix<_>) = m.Expr
      
    let mexprt (m:IMatrix<_>) = m.ExprT
    
    let mexpr2d (m:IMatrix<_>) = m.Expr2D
    
    let mdim0 (m:IMatrix<'t>) = m.Dims.[0]
    
    let mdim1 (m:IMatrix<'t>) = m.Dims.[1]

    let kr (i:int) (j:int) (m:IMatrix<_>) = if i = j then m.[i,j] else zero

    let elem (m:IMatrix<_>) = m |> mexpr |> Array.map(Array.map Scalar<_>)

    let elem2d (m:IMatrix<_>) = m |> mexpr2d |> Array2D.map Scalar<_>

    let mmap map (m:IMatrix<_>) = [| for i in 0..m.Dims.[0] - 1 -> [| for j in 0..m.Dims.[1] - 1 -> map i j m |] |]
    
    let msimplify (m:IMatrix<_>) = m |> mexpr2d |> Array2D.map simplifye |> Matrix<_>

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

    let mrswitch i j (l:Matrix<'t>) =
        do fail_if_invalid_row_indices [i;j] l
        let ri = l.[i] 
        let rj = l.[j]
        let rows = l.Rows.Clone() :?> Vector<'t> array
        rows.[i] <- rj
        rows.[j] <- ri
        Matrix<'t> rows

    let mraddmul i j (k:Scalar<'t>) (l:Matrix<'t>) =
        do fail_if_invalid_row_indices [i;j] l
        let rows = l.Rows.Clone() :?> Vector<'t> array
        let ri = l.[i] + k * l.[j] 
        rows.[i] <- ri
        Matrix<'t> rows

    let mcrepl j (v:Vector<_>) (l:Matrix<_>) =
        do 
            fail_if_invalid_col_index j l
            if v.Dim <> l.Dim0 then failwithf "The dimension of the column vector (%A) is not the same as the number of rows (%A)" v.Dim l.Dim0
        let cols = l.Columns.Clone() :?> Vector<'t> array
        cols.[j] <- v
        Matrix<'t>.ofCols cols
        
    let diag (m:IMatrix<_>) = 
        let dim = Math.Min(m.Dims.[0], m.Dims.[1]) in
        Array2D.init dim dim (fun i j -> kr i j m) |> Matrix<'t>

    let identmat<'t when 't: equality and 't :> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t>> n = 
        LinearAlgebraOps.identity_mat n |> Matrix<'t>

    let zeromat<'t when 't: equality and 't :> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t>> i j = 
        Array2D.init i j (fun _ _ -> zero<'t>) |> Matrix<'t>

    let mrdel n (m:IMatrix<_>) =
        do fail_if_invalid_row_index n m
        m.Expr |> Array.indexed |> Array.filter(fun (i, _) -> i <> n) |> Array.map snd |> Matrix<'t>.ofRows
    
    let mcdel n (m:IMatrix<_>) =
        do fail_if_invalid_col_index n m
        m |> mexprt |> Array.indexed |> Array.filter(fun (i, _) -> i <> (int) n) |> Array.map snd |> Matrix<'t>.ofCols
    
    let submat i j (m:IMatrix<_>) = m |> mrdel i |> mcdel j

    let mrzeros(m:IMatrix<_>) = m.Rows |> Array.map(velem >> LinearAlgebraOps.count_by((=) zero))

    let mczeros(m:IMatrix<_>) = m.Columns |> Array.map(velem >> LinearAlgebraOps.count_by((=) zero))

    let rec det (m:IMatrix<_>) =
        do fail_if_not_square m
        let n = m.Dims.[0] in
        let zri,zr = m |> mrzeros |> LinearAlgebraOps.maxi 
        let zci,zc = m |> mczeros|> LinearAlgebraOps.maxi
        match n with
        | 1 -> m.[0,0]
        | 2 -> m.[0,0] * m.[1,1] - m.[0,1] * m.[1, 0]
        | _ -> 
            if zr >= zc then 
                [| for j in 0..n - 1 -> m |> submat zri j |> det |> (*) ((negone***(zri + j)) * m.[zri, j]) |] |> Array.reduce (+) 
            else
                [| for i in 0..n - 1 -> m |> submat i zci |> det |> (*) ((negone***(i + zci)) * m.[i, zci]) |] |> Array.reduce (+) 

    let minor i j (m:IMatrix<_>) = m |> submat i j |> det

    let cofactor i j (m:IMatrix<_>) = m |> minor i j |> (*) (negone***(i+j))

    let coexpand_r i (m:IMatrix<_>) =
        [|for j in 0 .. m.Dims.[1] - 1 -> cofactor i j m |] |> Array.reduce (+)

    let coexpand_c j (m:IMatrix<_>) =
        [|for i in 0 .. m.Dims.[0] - 1 -> cofactor i j m |] |> Array.reduce (+)

    let comat m =  m |> mmap cofactor |> Matrix<_>

    let adjoint (m:IMatrix<_>) = m |> comat |> trans
        
    let inverse (m:IMatrix<_>) = adjoint m / det m

    let mblock i0 j0 i1 j1 (m:IMatrix<_>) = 
        do
            fail_if_invalid_row_indices [i0;i1] m
            fail_if_invalid_col_indices [j0;j1] m
        [|for i in i0 .. i1 -> [| for j in j0 .. j1 -> m.[i, j] |] |] |> Matrix<_>

    let mpart (rowgroup:int seq) (colgroup: int seq) (m:IMatrix<_>) = BlockMatrix<_>(rowgroup, colgroup, m)

    let mblocks (m:BlockMatrix<_>) = m.Blocks

    let is_square (m:IMatrix<_>) = m.Dims.[0] = m.Dims.[1]

    let is_ident (m:IMatrix<_>) = 
        m |> elem2d |> Array2D.forall(fun i j e -> if i = j then e = one else e = zero) 

    let is_zero (m:IMatrix<_>) = 
        m |> elem2d |> Array2D.forall(fun _ _ e -> e = zero) 

    let is_diag (m:IMatrix<_>) = 
           m |> elem2d |> Array2D.forall(fun i j e -> if i <> j then e = zero else true) 
         
    let is_upper_tri (m:IMatrix<_>) = 
             m |> elem2d |> Array2D.forall(fun i j e -> if i > j then e = zero else true) 
    
    let is_lower_tri (m:IMatrix<_>) = 
             m |> elem2d |> Array2D.forall(fun i j e -> if i < j then e = zero else true) 

    let diag_elem (m:IMatrix<_>) =
        [| for i in 0..m.Dims.[0] - 1 do [| for j in 0..m.Dims.[1] - 1 do if i = j then yield m.[i,j] |] |] |> 
            Array.filter(Array.length >> (<>) 0) |> Array.map(Array.item 0)
    
    let super_diag_elem (m:IMatrix<_>) =
        [| for i in 0..m.Dims.[0] - 1 do [| for j in 0..m.Dims.[1] - 1 do if j = i + 1 then yield m.[i,j] |] |] |> 
            Array.filter(Array.length >> (<>) 0) |> Array.map(Array.item 0)

    let supra_diag_elem (m:IMatrix<_>) =
        [| for i in 0..m.Dims.[0] - 1 do [| for j in 0..m.Dims.[1] - 1 do if j = i - 1 then yield m.[i,j] |] |] |> 
            Array.filter(Array.length >> (<>) 0) |> Array.map(Array.item 0)

    let jordan_block<'t when 't: equality and 't :> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t>> n (l:obj) = 
        Array2D.init n n (fun i j -> if i = j then sconst l else if j = i + 1 then one else zero) |> Matrix<'t>

    let is_jordan_block (m:IMatrix<_>) =
        let l = m.[0,0] in
        is_square m && m |> elem2d |> Array2D.forall(fun i j e -> if i = j then e = l else if j = i + 1 then e = one else e = zero)

    let jordan_blocks (m:JordanMatrix<_>) = m.JordanBlocks

    let perm_jordan_blocks (perm:seq<int>) (m:JordanMatrix<_>) = 
        if Seq.length perm <> m.JordanBlocks.Length then failwith "The length of the permutation sequence must be the same as the number of blocks in the Jordan matrix."
        m.JordanBlocks |> Array.permute(fun i -> Seq.item i perm) |> JordanMatrix<_>

    let jordan_block_eigenv (m:IMatrix<_>) =
        if is_jordan_block m then m.[0,0] else failwith "This matrix is not a Jordan block."

    let jordan_block_dim (m:IMatrix<_>) =
        if is_jordan_block m then m.Dims.[0] else failwith "This matrix is not a Jordan block."

    let jordan_mat<'t when 't: equality and 't :> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t>> (blocks:IMatrix<'t> seq) = 
        blocks |> Seq.toArray |> JordanMatrix<'t> 

    let is_jordan_mat (m:IMatrix<_>) = 
        if not (is_square m) || not (is_upper_tri m) then
            [||]
        else
            let mutable l = m.[0,0]
            let mutable bs = 0
            let blocks = 
                [|
                    for i in 0..mdim0 m - 1 do  
                        if  m.[i,i] <> l then
                            let block =  mblock bs bs i i m
                            bs <- i
                            l <- m.[i,i]
                            yield block
                |] 
            blocks
            //blocks |> Array.sumBy mdim0 = mdim0 m && blocks |> Array.sumBy mdim1 = mdim1 m 
            //&& Array.forall is_jordan_block blocks

    let mcharpoly (v:ScalarVar<'t>) (m:IMatrix<'t>) =
        fail_if_not_square m
        msub m (v * (identmat (mdim0 m))) |> det |> AlgebraOps.ratsimp == zero

    let mechelon (m:IMatrix<'t>) = m |> mexpr |> CAS.LinearAlgebra.echelon |> Matrix<'t>

    let jordan_normal_form (m:IMatrix<'t>) = 
        fail_if_not_square m
        m |> mexpr |> CAS.LinearAlgebra.jordan_normal_form 
        |> List.map(
            function 
            | l::m -> m |> List.map (fun n -> jordan_block<'t> ((ev >> to_int) n) (ev l) :> IMatrix<'t>)  
            | b -> failwithf "The returned list %A is not of the expected form for a Jordan block." b
        ) 
        |> List.concat 
        |> jordan_mat<'t>

    let jordan_similar (j:JordanMatrix<'t>) (m:IMatrix<'t>) = 
        fail_if_not_square m
        let blocklist = j |> jordan_blocks |> Array.map(fun b -> b.[0,0].Expr, exprv b.Dims.[0]) 
        m |> mexpr |> CAS.LinearAlgebra.jordan_similar blocklist |> Matrix<'t>

    (*
    let coeffmat (eqns:ScalarEquation<'t> seq) =
        let c =  eqns |> Seq.map (rhs >> sexpr >> get_vars) |> Seq.sumBy (List.length) in
        if c > 0 then failwith "The equation system must have only constants on its RHS to be put into matrix form."
        let vars = eqns |> Seq.map(lhs >> sexpr >> get_vars) |> List.concat
        let vn = vars |> List.map (fun v -> v.Name)
        let terms = 
               match eqn |> lhs |> sexpr |> simplifye with
               | LinearTerms vn t -> t
               | _ -> failwithf "%A is not a linear expression of variables %A." eqn.Rhs vn
        let b1 = 
            terms |> List.filter(function|[Constant c; VariableWithOneOfNames vn _] -> true | _ -> false) 
            |> List.map (
                function | [Constant c; VariableWithOneOfNames vn v] -> (v |> get_var |> exprvar<'t>), expand_as<'t> c | _ -> failwithf "Cannot determine the slope coefficient parameter symbol.")
            |> List.groupBy (fst >> sprinte)
        ()
    *)