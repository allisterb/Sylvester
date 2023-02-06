namespace Sylvester

open System

open FSharp.Quotations

open MathNet.Numerics

open Arithmetic
open Dimension
open Vector

[<StructuredFormatDisplay("{Display}")>]
type Matrix<'t when 't: equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable>
    internal(e: Expr<'t> array array) = 
    do if e |> Array.forall (fun a -> a.Length = e.[0].Length) |> not then failwith "The length of each column in a matrix must be the same."
    let expr = e  |> Array.map (Array.map expand_as<'t>)
    let exprmn = Array.map (Array.map MathNetExpr.fromQuotation) expr
    let exprlist = expr |> Array.map Array.toList |> Array.toList
    let exprt = expr  |> Ops.transpose_mat
  
    member val Expr = expr
    member val ExprMathNet = exprmn
    member val ExprT = exprt
    member val ExprList = exprlist
    member val ExprArray = expr |> array2D
    member val ExprVars = expr |> Array.map (Array.map(get_vars >> List.toArray)) |> Array.concat
    
    member val Rows = expr |> Array.map Vector<'t>
    member val Cols = exprt |> Array.map Vector<'t>
    member val RowsList = expr |> Array.map Vector<'t> |> Array.toList
    member val ColsList = exprt |> Array.map Vector<'t> |> Array.toList
    
    member x.Display = 
        let nl = System.Environment.NewLine
        x.Rows
        |> Array.skip 1 
        |> Array.fold(fun s e -> sprintf "%s%s%s" s (nl) (e.LinearDisplay)) (nl + x.Rows.[0].LinearDisplay) 
        |> sprintf "%s"
    
    member x.LinearDisplay = 
        let replace (o:string) n (s:string) = s.Replace(o, n) 
        x.Rows
        |> Array.skip 1 
        |> Array.fold(fun s e -> sprintf "%s, %s" s (e.Display)) (x.Rows.[0].Display) 
        |> replace "(" "["
        |> replace ")" "]"
        |> sprintf "[%s]"
    
    member x.Item with get(i) = x.Rows.[i] and set i value = Array.set x.Rows i value
    
    member x.AsNumeric() = 
        let t = typeof<'t>
        match t with
        | LinearAlgebraNumericOpType -> expr |> Array.map (Array.map evaluate) |> LinearAlgebra.DenseMatrix.ofRowArrays
        | _ -> failwithf "The type %A is not compatible with numeric linear algebra operations." t
    
    interface IPartialShape<``2``> with
        member val Rank = Some 1 with get,set
        member val Dims = [| Convert.ToInt64(e.Length) |] |> Some with get,set
    
    new([<ParamArray>] v:'t array array) = let expr = v |> Array.map(Array.map(fun e -> <@ e @>)) in Matrix<'t>(expr)
    
    new(v: Expr<'t list list>) = let expr = v |> expand_lists' |> List.map(List.toArray) |> List.toArray in Matrix<'t>(expr)
    
    new(d:'t list list) = Matrix<'t>((List.map(List.toArray) >> List.toArray) d)
    
    new(d: Expr<'t> [,]) = let d' = d |> Array2D.toJagged in Matrix<'t> d'
    
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
    member x.Cols = x.ExprT |> Array.map Vector<'dim0, 't>
    member x.Rows = x.Expr |> Array.map Vector<'dim1, 't>  
    member x.Transpose = Matrix<'dim1, 'dim0, 't>(x.ExprT)
    member val Display = base.Display
    member val LinearDisplay = base.LinearDisplay
    
    member x.Item(i: int) = x.Rows.[i]
    
    member x.Kr = fun (i:int) (j:int) -> if i = j then x.[i].[j] else expand_as<'t>(zero_val(typeof<'t>)) |> Scalar<'t>
    
    interface IMatrix<'dim0, 'dim1> with 
        member val Dim0 = dim0
        member val Dim1 = dim1
    
    new([<ParamArray>] v:'t array array) = let expr = v |> Array.map(Array.map(fun e -> <@ e @>)) in Matrix<'dim0, 'dim1, 't>(expr)
    
    new(d: Expr<'t> [,]) = let d' = d |> Array2D.toJagged in Matrix<'dim0, 'dim1, 't> d'
    
    new(v: Expr<'t list list>) = let expr = v |> expand_lists' |> List.map(List.toArray) |> List.toArray in Matrix<'dim0, 'dim1, 't>(expr)
    
    new(d:'t list list) = Matrix<'dim0, 'dim1, 't>((List.map(List.toArray) >> List.toArray) d)
    
    new(rows: Vector<'dim1, 't> array) = let e = rows |> Array.map(fun a -> a.Expr) in Matrix<'dim0, 'dim1, 't>(e)
    
    new (v:Expr<'t>) = 
        let e = Array.create (number<'dim0>.IntVal) (Array.create (number<'dim1>.IntVal) v) in 
            Matrix<'dim0, 'dim1, 't> e
    new(data:Expr<'t list>) = 
        let d = data |> expand_list' |> List.toArray |> Array.chunkBySize (number<'dim0>.IntVal)
        Matrix<'dim0, 'dim1, 't> d
    new (_:'dim0, _:'dim1, data:Expr<'t> [] []) = Matrix<'dim0, 'dim1, 't> data
    
    static member ofRows(data: Expr<'t list list>) = Matrix<'dim0, 'dim1,'t>(data)
      
    static member ofCols(data: Expr<'t list list>) = Matrix<'dim0, 'dim1,'t>(data |> expand_lists' |> Ops.mat_to_array |> Ops.transpose_mat)

    static member ofRows (data:Expr<'t> [] []) = Matrix<'dim0, 'dim1, 't>(data)
    
    static member ofCols (data:Expr<'t> [] []) = Matrix<'dim0, 'dim1, 't>(data |> Ops.transpose_mat)

    static member Zero:Matrix<'dim0, 'dim1, 't> = let e = Array.create (number<'dim0>.IntVal) (Array.create (number<'dim1>.IntVal) (zero_val(typeof<'t>) |> expand_as<'t>)) in Matrix<'dim0, 'dim1, 't> e

    static member One:Matrix<'dim0, 'dim1, 't> = let e = Array.create (number<'dim0>.IntVal) (Array.create (number<'dim1>.IntVal) (one_val(typeof<'t>) |> expand_as<'t>)) in Matrix<'dim0, 'dim1, 't> e

    static member (+) (l: Matrix<'dim0, 'dim1, 't>, r: Matrix<'dim0, 'dim1, 't>) = 
        let m = Array.map2 (+) l.Rows r.Rows in Matrix<'dim0, 'dim1, 't> m
           
    static member (-) (l: Matrix<'dim0, 'dim1, 't>, r: Matrix<'dim0, 'dim1, 't>) = 
        let m = Array.map2 (-) l.Rows r.Rows in Matrix<'dim0, 'dim1, 't> m

    static member (*) (l: Scalar<'t>, r: Matrix<'dim0, 'dim1, 't>) = 
        let m = r.Rows |> Array.map ((*) l) in Matrix<'dim0, 'dim1, 't> m

    static member (*) (l: Matrix<'dim0, 'dim1, 't>, r: Scalar<'t>) = 
         let m = l.Rows |> Array.map (fun v -> v * r) in Matrix<'dim0, 'dim1, 't> m

    static member (*) (l: 't, r: Matrix<'dim0, 'dim1, 't>) = scalar l * r

    static member (*) (l: Matrix<'dim0, 'dim1, 't>, r: 't) = l * scalar r
    
    static member (~-) (l: Matrix<'dim0, 'dim1, 't>) = 
        let m = l.Rows |> Array.map (~-) in Matrix<'dim0, 'dim1, 't> m

    static member (*) (l:Matrix<'dim0, 'dim1, 't>, r:Vector<'dim1, 't>) =
        [| for i in 0..l.Dim0.IntVal - 1 -> l.Rows.[i] * r |] |> Array.map sexpr |> Vector<'dim0, 't>

    static member (*) (l:Matrix<'dim0, 'dim1, 't>, r:Matrix<'dim1, 'dim2, 't> ) =             
        [| for i in 0..r.Dim1.IntVal - 1 -> l * r.Cols.[i] |] |> Array.map vexpr  |> Ops.transpose_mat |> Matrix<'dim0, 'dim2, 't>

type SquareMatrix<'dim0, 't when 'dim0 :> Number  and 't : equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable> = Matrix<'dim0, 'dim0, 't>
type Mat<'dim0, 'dim1 when 'dim0 :> Number and 'dim1:> Number> = Matrix<'dim0, 'dim1, real>
type MatC<'dim0, 'dim1 when 'dim0 :> Number and 'dim1:> Number> = Matrix<'dim0, 'dim1, complex>
type MatQ<'dim0, 'dim1 when 'dim0 :> Number and 'dim1:> Number> = Matrix<'dim0, 'dim1, rat>
type MatZ<'dim0, 'dim1 when 'dim0 :> Number and 'dim1:> Number> = Matrix<'dim0, 'dim1, int>

module Matrix =
    let (|MatrixR|_|) (m:Matrix<_,_,_>) = m.RowsList |> Some

    let (|MatrixC|_|) (m:Matrix<_,_,_>) = m.ColsList |> Some

    let marray (m:Matrix<_,_,_>) = m.Expr

    let marrayi (m:Matrix<_,_,_>) = m.Expr |> Array.indexed

    let marray' (m:Matrix<_,_,_>) = m.ExprT

    let marrayi' (m:Matrix<_,_,_>) = m.ExprT |> Array.indexed

    let _mat (l:'dim0) (r:'dim1) (data:Expr<'t> [] []) = Matrix<'dim0, 'dim1, 't> data
    
    let _mat' (l:'dim0) (r:'dim1) (data:Expr<'t> [] []) = Matrix<'dim0, 'dim1, 't>.ofCols data

    let mat (l:'dim0) (r:'dim1) (data:Expr<'t list>) = Matrix<'dim0, 'dim1, 't> data
    
    let mat' (l:'dim0) (r:'dim1) (data:Expr<'t list>) = 
        let d = data |> expand_list' |> List.toArray |> Array.chunkBySize (number<'dim1>.IntVal)
        Matrix<'dim0, 'dim1, 't>.ofCols d

    let matl (l:'dim0) (r:'dim1) (data:Expr<'t list list>) = Matrix<'dim0, 'dim1, 't> data
    
    let matl' (l:'dim0) (r:'dim1) (data:Expr<'t list list>) = Matrix<'dim0, 'dim1, 't>.ofCols data
    
    let mata (l:'dim0) (r:'dim1) (data:Expr<'t> array array) = Matrix<'dim0, 'dim1, 't> data
    
    let mata' (l:'dim0) (r:'dim1) (data:Expr<'t> array array) = Matrix<'dim0, 'dim1, 't>.ofCols data

    let inline (|+||) (l:Matrix<'dim0, 'dim1, 't>) (r:Vector<'dim0, 't>) = 
        let data = Array.append l.Cols [|r|] |> Array.map vexpr |> array2D |> Array2D.transpose |> Array2D.toJagged 
        Matrix<_,_,'t> (l.Dim0, (pp (l.Dim1 + ``1``)), data)

    let inline (||+|) (l:Vector<'dim0, 't>) (r:Matrix<'dim0, 'dim1, 't>)  = 
        let data = Array.append [|l|] r.Cols |> Array.map vexpr |> array2D |> Array2D.transpose |> Array2D.toJagged 
        Matrix<_,_,'t> (l.Dim0, (pp (r.Dim1 + ``1``)), data)

    let inline (||+||) (l:Matrix<'dim0, 'dim1, 't>) (r:Vector<'dim1, 't>) = 
        Array.append l.Rows [|r|] |> Array.map vexpr |> array2D |> Array2D.toJagged |> _mat (pp (l.Dim0 + ``1``)) l.Dim1 

    let mident<'dim0, 't when 'dim0 :> Number and 't : equality and 't:> ValueType and 't : struct and 't: (new: unit -> 't) and 't :> IEquatable<'t> and 't :> IFormattable> = 
        Ops.identity_mat<'t> (number<'dim0>.IntVal) |> Matrix<'dim0, 'dim0, 't>

    let mtrans (m:Matrix<'dim0, 'dim1, 't>) = m.Transpose

    let madd (l:Matrix<'dim0, 'dim1, 't>) (r:Matrix<'dim0, 'dim1, 't>) = l + r
    
    let msub (l:Matrix<'dim0, 'dim1, 't>) (r:Matrix<'dim0, 'dim1, 't>) = l - r
    
    let msmul (l:'t) (r:Matrix<'dim0, 'dim1, 't>) = Matrix<'dim0, 'dim1, 't>.(*) (l, r)

    let msimplify (l:Matrix<'dim0, 'dim1, 't>) = l.Expr |> Array.map (Array.map simplify') |> Matrix<'dim0, 'dim1, 't>

    let inline mrmul (l:Matrix<'dim0, 'dim1, 't>) i (k:Expr<'t>) =
        check (i +< l.Dim0)
        let rows = l.Rows.Clone() :?> Vector<'dim1, 't> array
        let ri = Scalar k * l.[int i]
        rows.[int i] <- ri
        Matrix<'dim0, 'dim1, 't> rows

    let inline mrswitch (l:Matrix<'dim0, 'dim1, 't>) i j =
        check (i +< l.Dim0)
        check (j +< l.Dim1)
        let ri = l.[int i] 
        let rj = l.[int j]
        let rows = l.Rows.Clone() :?> Vector<'dim1, 't> array
        rows.[(int) i] <- rj
        rows.[(int) j] <- ri
        Matrix<'dim0, 'dim1, 't> rows
    
    let inline mraddmul (l:Matrix<'dim0, 'dim1, 't>) i j (k:Expr<'t>) =
         check (i +< l.Dim0)
         check (j +< l.Dim1)
         let rows = l.Rows.Clone() :?> Vector<'dim1, 't> array
         let ri = l.[int i] + Scalar k * l.[int j] 
         rows.[int i] <- ri
         Matrix<'dim0, 'dim1, 't> rows

    let inline mdiag (l:Matrix<'dim0,'dim1,'t>) = 
        let dim = Math.Min(l.Dim0.IntVal, l.Dim1.IntVal)
        Array2D.init dim dim l.Kr
        |> Array2D.toJagged
        |> sexprs'
        |> _mat' (min l.Dim0 l.Dim1) (min l.Dim0 l.Dim1)

    let inline mdelr n (m:Matrix<_,_,_>) =
        check (n +< m.Dim0)
        m |> marrayi |> Array.filter(fun (i, r) -> i <> (int) n) |> Array.map snd |> mata (pp (m.Dim0 - ``1``)) m.Dim1
    
    let inline mdelc n (m:Matrix<_,_,_>) =
        check (n +< m.Dim1)
        m |> marrayi' |> Array.filter(fun (i, _) -> i <> (int) n) |> Array.map snd |> mata' m.Dim0 (pp (m.Dim1 - ``1``))
    
    let det (l:SquareMatrix<'dim0, _>) =
        match l.Dim0.IntVal with
        | 1 -> l.[0].[0]
        | 2 -> l.[0].[0] * l.[1].[1] - l.[0].[1] * l.[1].[0]
        | _ -> failwith "Not supported" 


    